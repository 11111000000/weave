;;; weave-parse.el --- High-level patch parsing over Org  -*- lexical-binding: t; -*-

;;; Commentary:
;; Use org-element to locate llm_patch blocks; dispatch to format-specific parsers.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'subr-x)
(require 'weave-registry)
(require 'weave-core)

(defun weave-parse--signature-at (beg end)
  "Return first non-empty line between BEG and END."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((sig nil))
        (while (and (not sig) (not (eobp)))
          (let ((line (string-trim-right (buffer-substring-no-properties
                                          (line-beginning-position)
                                          (line-end-position)))))
            (unless (string-empty-p line) (setq sig line)))
          (forward-line 1))
        sig))))

(defun weave-parse-patch-at-point ()
  "Parse llm_patch block at point. Return (values patch diagnostics)."
  (pcase (weave-org-patch-bounds-at-point)
    (`(,beg . ,end) (weave-parse-patch-bounds beg end))
    (_ (list nil (list (weave-core-make-diagnostic
                        :severity 'error :code :bad-signature
                        :message "Point not in llm_patch block"))))))

(defun weave-parse-patch-bounds (beg end)
  "Parse llm_patch between BEG and END. Return (values patch diagnostics)."
  (let ((sig (weave-parse--signature-at beg end)))
    (if (not sig)
        (list nil (list (weave-core-make-diagnostic
                         :severity 'error :code :bad-signature
                         :message "Empty block")))
      (let ((parser (weave-dispatch-parser sig)))
        (if (not parser)
            (list nil (list (weave-core-make-diagnostic
                             :severity 'error :code :bad-signature
                             :message (format "Unknown signature: %s" sig))))
          (funcall parser beg end))))))

(defun weave-parse-all-in-buffer ()
  "Return list of (patch . diagnostics) for all llm_patch blocks in current buffer."
  (let (out)
    (org-with-wide-buffer
     (org-element-map (org-element-parse-buffer) 'special-block
       (lambda (el)
         (when (string= (org-element-property :type el) "llm_patch")
           (let* ((beg (org-element-property :contents-begin el))
                  (end (org-element-property :contents-end el)))
             (push (apply #'cons (weave-parse-patch-bounds beg end)) out))))))
    (nreverse out)))

(provide 'weave-parse)
;;; weave-parse.el ends here
