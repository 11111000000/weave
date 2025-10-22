;;; weave-log.el --- Logging and preview helpers for Weave  -*- lexical-binding: t; -*-

;;; Commentary:
;; Cheap-to-disable logging and confirmation UI.

;;; Code:

(require 'cl-lib)

(defgroup weave-log nil
  "Logging settings for Weave."
  :group 'weave)

(defcustom weave-log-level nil
  "Logging level: nil, 'info, or 'debug."
  :type '(choice (const :tag "Off" nil) (const info) (const debug))
  :group 'weave-log)

(defconst weave--log-buffer "*weave-log*")

(defun weave-log--enabled-p (lvl)
  "Return non-nil if LVL logging is enabled."
  (pcase weave-log-level
    ('debug t)
    ('info (memq lvl '(info)))
    (_ nil)))

(defun weave-log (lvl fmt &rest args)
  "Log message at level LVL with FMT and ARGS."
  (when (weave-log--enabled-p lvl)
    (with-current-buffer (get-buffer-create weave--log-buffer)
      (goto-char (point-max))
      (insert (apply #'format (concat (format-time-string "[%F %T] ") fmt) args) "\n"))))

(defun weave-log-preview (plan)
  "Show an informative preview buffer for PLAN: files, operations and diff/fragment."
  (let ((buf (get-buffer-create "*weave-preview*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Patch: %s\n\n" (alist-get :patch-id plan)))
        (dolist (fp (alist-get :files plan))
          (insert (format "--- %s ---\n" (alist-get :file fp)))
          (dolist (o (alist-get :ops fp))
            (let ((op (alist-get :op o)))
              (pcase op
                ('write
                 (insert "[write] file will be overwritten/created.\n")
                 (let ((content (alist-get :_write-content fp)))
                   (when content
                     (insert "New content (start):\n"
                             (string-join
                              (seq-take (split-string content "\n") 10) "\n")
                             (if (> (length (split-string content "\n")) 10)
                                 "\n...\n[end] ...\n" "\n")))))
                ('delete
                 (insert "[delete] file will be removed.\n"))
                ('edit
                 (insert "[edit] file will be patched.\n")
                 (let ((old (and (alist-get :_mtime fp)
                                 (car (weave-fs-read-file
                                       (expand-file-name (alist-get :file fp)
                                                         (weave-path-project-root))))))
                       (new (alist-get :_new-text fp)))
                   (when (and old new)
                     (insert "Before:\n"
                             (string-join (seq-take (split-string old "\n") 10) "\n")
                             (if (> (length (split-string old "\n")) 10) "\n...\n" "\n"))
                     (insert "After :\n"
                             (string-join (seq-take (split-string new "\n") 10) "\n")
                             (if (> (length (split-string new "\n")) 10) "\n...\n" "\n")))))))
            (insert "\n")))
        (view-mode 1))
      (display-buffer buf)))

  (defun weave-log-confirm-apply (plan)
    "Ask user confirmation to apply PLAN, showing a short summary."
    (weave-log-preview plan)
    (y-or-n-p (format "Apply patch %s? " (alist-get :patch-id plan))))

  (provide 'weave-log)
;;; weave-log.el ends here
