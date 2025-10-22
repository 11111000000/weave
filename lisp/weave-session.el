;;; weave-session.el --- Track last generated patch groups  -*- lexical-binding: t; -*-

;;; Commentary:
;; Keep track of recent inserted patch blocks for group-apply.

;;; Code:

(require 'cl-lib)
(require 'weave-org)
(require 'weave-parse)
(require 'weave-plan)
(require 'weave-apply)
(require 'weave-path)

(defvar-local weave--last-group nil
  "Markers for last generated group in this buffer: (beg-marker . end-marker).")

(defun weave-session-begin-group ()
  "Mark start of a new generated group at point."
  (setq weave--last-group (cons (copy-marker (point) t) (copy-marker (point) t))))

(defun weave-session-finish-group ()
  "Finish marking group end at point."
  (when weave--last-group
    (set-marker (cdr weave--last-group) (point))
    weave--last-group))

;;;###autoload
(defun weave-apply-last-generated ()
  "Apply all llm_patch blocks in the last generated group."
  (interactive)
  (unless weave--last-group
    (user-error "No generated group in this buffer"))
  (let* ((beg (marker-position (car weave--last-group)))
         (end (marker-position (cdr weave--last-group)))
         (root (weave-path-project-root))
         (applied 0) (errors 0))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "^#\\+begin_llm_patch\\b" end t)
        (let* ((ctx (weave-org-patch-bounds-at-point)))
          (when ctx
            (pcase-let ((`(,p ,diags) (weave-parse-patch-bounds (car ctx) (cdr ctx))))
              (if (not p) (setq errors (1+ errors))
                (let ((plan (weave-plan p root)))
                  (if (alist-get :ok plan)
                      (progn (weave-apply plan) (setq applied (1+ applied)))
                    (setq errors (1+ errors))))))))))
    (message "Weave: applied %d, errors %d" applied errors)))

(provide 'weave-session)
;;; weave-session.el ends here
