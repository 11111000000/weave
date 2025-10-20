;;; weave-apply.el --- Transactional application of Weave plans  -*- lexical-binding: t; -*-

;;; Commentary:
;; Apply a PLAN produced by weave-plan with all-or-nothing semantics.

;;; Code:

(require 'cl-lib)
(require 'weave-fs)
(require 'weave-path)
(require 'weave-plan)
(require 'weave-log)

(defun weave-apply (plan)
  "Apply PLAN transactionally. Return (t . report) or (nil . error)."
  (unless (alist-get :ok plan)
    (cons nil 'not-ok))
  (let* ((root (weave-path-project-root))
         (tmpdir (make-temp-file "weave-apply-" t))
         (staged (make-hash-table :test 'equal))
         (backups (make-hash-table :test 'equal))
         commit-ok)
    (unwind-protect
        (progn
          ;; Stage new file contents for write/edit
          (dolist (fp (alist-get :files plan))
            (let* ((rel (alist-get :file fp))
                   (abs (expand-file-name rel root))
                   (ops (alist-get :ops fp)))
              (pcase (and ops (alist-get :op (car ops)))
                ('write
                 (let ((content (alist-get :content (car (last ops)))))
                   ;; no content in plan, this comes from patch; skip here
                   (puthash rel (cons :write content) staged)))
                (_
                 ;; edit: use computed new text from plan
                 (let ((new (alist-get :_new-text fp)))
                   (when new (puthash rel (cons :edit new) staged)))))))
          ;; Write staged files
          (maphash
           (lambda (rel pair)
             (let* ((abs (expand-file-name rel root))
                    (kind (car pair)) (content (cdr pair))
                    (bak (and (file-exists-p abs) (weave-fs-backup-path root rel))))
               (when bak
                 (copy-file abs bak t)
                 (puthash rel bak backups))
               (when content
                 (weave-fs-write-atomic abs content))))
           staged)
          ;; Deletes
          (dolist (fp (alist-get :files plan))
            (let* ((rel (alist-get :file fp))
                   (abs (expand-file-name rel root))
                   (ops (alist-get :ops fp)))
              (dolist (o ops)
                (when (and (eq (alist-get :op o) 'delete)
                           (alist-get :will-delete o)
                           (file-exists-p abs))
                  (let ((bak (weave-fs-backup-path root rel)))
                    (copy-file abs bak t)
                    (puthash rel bak backups)
                    (delete-file abs))))))
          (setq commit-ok t)
          (cons t `(:backups ,backups :staged ,staged)))
      (unless commit-ok
        ;; Attempt minimal rollback by restoring backups
        (maphash
         (lambda (rel bak)
           (ignore-errors
             (let* ((abs (expand-file-name rel root)))
               (when (file-exists-p bak)
                 (copy-file bak abs t)))))
         backups)
        (ignore-errors (delete-directory tmpdir t))))))

(provide 'weave-apply)
;;; weave-apply.el ends here
