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
  "Show a simple preview buffer for PLAN."
  (let ((buf (get-buffer-create "*weave-preview*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Patch: %s\n\n" (alist-get :patch-id plan)))
        (dolist (fp (alist-get :files plan))
          (insert (format "- %s\n" (alist-get :file fp)))
          (dolist (o (alist-get :ops fp))
            (insert (format "  * %s %s\n"
                            (alist-get :op o)
                            (or (alist-get :kind o) ""))))))
      (view-mode 1))
    (display-buffer buf)))

(defun weave-log-confirm-apply (plan)
  "Ask user confirmation to apply PLAN, showing a short summary."
  (weave-log-preview plan)
  (y-or-n-p (format "Apply patch %s? " (alist-get :patch-id plan))))

(provide 'weave-log)
;;; weave-log.el ends here
