;;; weave-path.el --- Project root and path safety for Weave  -*- lexical-binding: t; -*-

;;; Commentary:
;; Resolve project root, normalize and validate relative paths.

;;; Code:

(require 'cl-lib)
(require 'project)

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
Return (values REL-NORM diagnostics)."
  (let ((abs (expand-file-name rel root)))
    (cond
     ((file-name-absolute-p rel)
      (list nil (list (list :severity 'error :code :absolute-path :message "Absolute path not allowed"))))
     ((string-match-p "\\=\\(?:\\./\\|\\)\\(?:.*\\(?:/\\|\\)\\)\\..\\(?:/\\|\\|\\)\\'" rel)
      (list nil (list (list :severity 'error :code :parent-dir :message "Parent dir '..' not allowed"))))
     ((not (string-prefix-p (file-name-as-directory root) (file-name-as-directory abs)))
      (list nil (list (list :severity 'error :code :bad-path :message "Path escapes project root"))))
     (t (list (file-relative-name abs root) nil)))))

(provide 'weave-path)
;;; weave-path.el ends here
