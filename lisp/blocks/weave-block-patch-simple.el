;;; weave-block-patch-simple.el --- Parser for PATCH SIMPLE v1  -*- lexical-binding: t; -*-

;;; Commentary:
;; Lex + FSM parse of PATCH SIMPLE v1 inside llm_patch Org special blocks.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'weave-core)
(require 'weave-registry)

(cl-defstruct (weave--ps-token (:constructor weave--ps-token))
  kind val line col pos)

(defun weave--ps--read-lines (beg end)
  "Yield list of lines with positions between BEG and END."
  (let (acc)
    (save-excursion
      (goto-char beg)
      (let ((ln 1))
        (while (< (point) end)
          (let* ((lbeg (point))
                 (lend (min end (line-end-position)))
                 (txt (buffer-substring-no-properties lbeg lend)))
            (push (list ln lbeg txt) acc)
            (setq ln (1+ ln))
            (goto-char (min end (1+ lend))))))
      (nreverse acc))))

(defun weave--ps--meta-line-p (s)
  "Return (key . value) for meta line S like \"key: value\", or nil."
  (when (string-match "\\=\\([[:alnum:]_-]+\\):[ \t]*\\(.*\\)\\'" s)
    (cons (downcase (match-string 1 s)) (match-string 2 s))))

(defun weave--ps--section-begin (s)
  "If S is a section begin, return (NAME . TAG) else nil."
  (when (string-match "\\=---\\([A-Z]+\\)\\(?:[ \t]+tag:\\([[:alnum:]_-]+\\)\\)?---\\'" s)
    (cons (match-string 1 s) (match-string 2 s))))

(defun weave--ps--section-end (s)
  "If S is a section end, return (NAME . TAG) else nil."
  (when (string-match "\\=---END[ \t]+\\([A-Z]+\\)\\(?:[ \t]+tag:\\([[:alnum:]_-]+\\)\\)?---\\'" s)
    (cons (match-string 1 s) (match-string 2 s))))

(defun weave--ps--collect-section (lines i name tag)
  "Collect payload lines starting at index I for section NAME with TAG.
Return (payload next-index diagnostics)."
  (let ((start (1+ i)) (buf (generate-new-buffer " /weave-ps/"))
        (found-end nil) diags)
    (unwind-protect
        (progn
          (cl-loop for j from start below (length lines)
                   for (_ lbeg txt) = (nth j lines)
                   do (let ((end (weave--ps--section-end (string-trim-right txt))))
                        (if (and end (string= (car end) name)
                                 (equal (cdr end) tag))
                            (progn (setq found-end j) (cl-return))
                          (with-current-buffer buf
                            (insert txt "\n")))))
          (unless found-end
            (push (weave-core-make-diagnostic
                   :severity 'error :code :missing-section
                   :message (format "Section %s missing END" name))
                  diags))
          (let ((payload (with-current-buffer buf
                           (buffer-substring-no-properties (point-min) (point-max)))))
            (when (> (string-bytes payload) weave--max-section-bytes)
              (push (weave-core-make-diagnostic
                     :severity 'error :code :missing-section
                     :message "Section exceeds maximum size") diags))
            (list payload (if found-end (1+ found-end) (length lines)) diags)))
      (kill-buffer buf))))

(defun weave--ps-parse (beg end)
  "Parse PATCH SIMPLE v1 block between BEG and END.
Return (values patch diagnostics)."
  (let* ((lines (weave--ps--read-lines beg end))
         (sig (caddr (car lines)))
         (diags nil)
         (idx 1)
         (meta (make-hash-table :test 'equal))
         ops curr-file curr-op changes)
    (unless (string= (string-trim-right sig) weave--format-signature)
      (push (weave-core-make-diagnostic
             :severity 'error :code :bad-signature
             :message "Expected PATCH SIMPLE v1") diags)
      (cl-return-from weave--ps-parse (list nil diags)))
    ;; Meta lines until first "file:" or EOF
    (cl-loop while (< idx (length lines)) do
             (pcase-let ((`(_ _ ,txt) (nth idx lines)))
               (let ((t (weave--ps--meta-line-p (string-trim-right txt))))
                 (if (and t (not (string-prefix-p "file:" (downcase txt))))
                     (puthash (car t) (cdr t) meta)
                   (cl-return))))
             (setq idx (1+ idx)))
    (let* ((id (gethash "id" meta)))
      (unless (and id (not (string-empty-p id)))
        (push (weave-core-make-diagnostic
               :severity 'error :code :missing-id :message "Missing id:") diags)))
    ;; Operations
    (while (< idx (length lines))
      (pcase-let ((`(_ _ ,txt) (nth idx lines)))
        (cond
         ((string-match "\\=file:[ \t]*\\(.+\\)\\'" txt)
          (when curr-op
            ;; flush previous op
            (push (pcase curr-op
                    ('write `(:type write :file ,curr-file :content ,(car changes)))
                    ('delete `(:type delete :file ,curr-file))
                    ('edit   `(:type edit :file ,curr-file :changes ,(nreverse changes)))
                    (_ nil))
                  ops)
            (setq changes nil curr-op nil))
          (setq curr-file (string-trim (match-string 1 txt)))
          (setq idx (1+ idx)))
         ((string-match "\\=op:[ \t]*\\([[:lower:]_]+\\)\\'" txt)
          (let ((op (intern (match-string 1 txt))))
            (unless (memq op '(write delete edit))
              (push (weave-core-make-diagnostic
                     :severity 'error :code :unknown-op
                     :message (format "Unknown op: %s" op)) diags))
            (setq curr-op op))
          (setq idx (1+ idx)))
         ((string-match "\\=---CONTENT\\(?:[ \t]+tag:\\([[:alnum:]_-]+\\)\\)?---\\'" txt)
          (if (not (eq curr-op 'write))
              (progn
                (push (weave-core-make-diagnostic
                       :severity 'error :code :missing-op
                       :message "CONTENT outside write op") diags)
                (setq idx (1+ idx)))
            (pcase-let ((`(,payload ,next ,dd) (weave--ps--collect-section lines idx "CONTENT" (match-string 1 txt))))
              (setq diags (nconc diags dd))
              (setq changes (list payload))
              (setq idx next))))
         ((string-match "\\=---FIND\\(?:[ \t]+tag:\\([[:alnum:]_-]+\\)\\)?---\\'" txt)
          (if (not (eq curr-op 'edit))
              (progn
                (push (weave-core-make-diagnostic
                       :severity 'error :code :missing-op
                       :message "FIND outside edit op") diags)
                (setq idx (1+ idx)))
            (pcase-let ((`(,find ,next1 ,dd1) (weave--ps--collect-section lines idx "FIND" (match-string 1 txt))))
              (setq diags (nconc diags dd1))
              (setq idx next1)
              (pcase-let* ((`(_ _ ,txt2) (nth idx lines)))
                (unless (string-match "\\=---WITH\\(?:[ \t]+tag:\\([[:alnum:]_-]+\\)\\)?---\\'" txt2)
                  (push (weave-core-make-diagnostic
                         :severity 'error :code :missing-section
                         :message "WITH expected after FIND") diags))
                (pcase-let ((`(,with ,next2 ,dd2) (weave--ps--collect-section lines idx "WITH" (match-string 1 txt2))))
                  (setq diags (nconc diags dd2))
                  (let ((kind (weave-core-classify-change find with)))
                    (push `(:find ,find :with ,with :kind ,kind) changes))
                  (setq idx next2))))))
         ((string-blank-p txt)
          (setq idx (1+ idx)))
         (t
          ;; Unknown line in op region: warning
          (setq idx (1+ idx))))))
    ;; flush last op
    (when curr-op
      (push (pcase curr-op
              ('write `(:type write :file ,curr-file :content ,(car changes)))
              ('delete `(:type delete :file ,curr-file))
              ('edit   `(:type edit :file ,curr-file :changes ,(nreverse changes)))
              (_ nil))
            ops))
    (let* ((patch `(:id ,(or (gethash "id" meta) "")
                        :title ,(gethash "title" meta)
                        :ops ,(nreverse ops)
                        :range (,beg . ,end)
                        :signature ,weave--format-signature)))
      (list patch diags))))

;; Register parser
(weave-register-parser weave--format-signature
                       (lambda (beg end) (weave--ps-parse beg end)))

(provide 'weave-block-patch-simple)
;;; weave-block-patch-simple.el ends here
