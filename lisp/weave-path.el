;;; weave-path.el --- Project root and path safety for Weave  -*- lexical-binding: t; -*-

;;; Commentary:
;; Resolve project root, normalize and validate relative paths.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'subr-x)

(defgroup weave-path nil
  "Path handling for Weave."
  :group 'weave)

(defcustom weave-autocreate-dirs t
  "When non-nil, auto-create missing directories on apply (with warning)."
  :type 'boolean
  :group 'weave-path)

(defun weave-path-project-root ()
  "Return project root directory as absolute path, trailing slash."
  (let* ((proj (project-current nil))
         (root (or (and proj (project-root proj))
                   (file-name-directory (or (buffer-file-name) default-directory)))))
    (file-name-as-directory (expand-file-name root))))

(defun weave-path-validate-rel (rel root)
  "Validate REL is a safe relative path under ROOT.
Return (values REL-NORM diagnostics) where REL-NORM is normalized relative path."
  (cond
   ;; Empty or non-string
   ((or (null rel) (not (stringp rel)) (string-empty-p rel))
    (list nil (list (list :severity 'error :code :bad-path :message "Empty or invalid path"))))
   ;; Absolute not allowed
   ((file-name-absolute-p rel)
    (list nil (list (list :severity 'error :code :absolute-path :message "Absolute path not allowed"))))
   (t
    (let* ((components (split-string rel "/" t))
           ;; For v1 forbid parent dir components; allow "." implicitly by normalization
           (has-parent (cl-some (lambda (seg) (string= seg "..")) components)))
      (if has-parent
          (list nil (list (list :severity 'error :code :parent-dir :message "Parent dir '..' not allowed"))))
      (let* ((root-true (file-name-as-directory (file-truename (expand-file-name root))))
             ;; Expand against root, then resolve symlinks via truename
             (abs (expand-file-name rel root))
             (abs-true (file-truename abs)))
        ;; Ensure resolved path stays under resolved root (blocks symlink escape)
        (if (not (string-prefix-p root-true (file-name-as-directory abs-true)))
            (list nil (list (list :severity 'error :code :symlink :message "Resolved path escapes project root (symlink)")))
          ;; Return normalized relative path against resolved root
          (list (file-relative-name abs-true root-true) nil)))))))


(provide 'weave-path)
;;; weave-path.el ends here
