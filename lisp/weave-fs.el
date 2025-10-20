;;; weave-fs.el --- File I/O utilities for Weave  -*- lexical-binding: t; -*-

;;; Commentary:
;; Thin file helpers: read/write text, atomic replace, backups, hashes.

;;; Code:

(require 'cl-lib)
(require 'weave-path)

(defgroup weave-fs nil
  "Filesystem policies for Weave."
  :group 'weave)

(defcustom weave-backup-dir nil
  "Directory for backups. When nil, use a temp dir per apply."
  :type '(choice (const :tag "Temporary per-apply directory" nil) directory)
  :group 'weave-fs)

(defun weave-fs-read-file (abs)
  "Read file ABS as UTF-8 string. Return (values text mtime size)."
  (when (file-exists-p abs)
    (let* ((attrs (file-attributes abs))
           (mtime (file-attribute-modification-time attrs))
           (size  (file-attribute-size attrs)))
      (list (with-temp-buffer
              (setq buffer-file-coding-system 'utf-8-unix)
              (insert-file-contents-literally abs nil nil nil t)
              (buffer-string))
            mtime size))))

(defun weave-fs-write-atomic (abs content)
  "Write CONTENT to ABS atomically, creating directories if needed."
  (let ((dir (file-name-directory abs)))
    (unless (file-directory-p dir)
      (when weave-autocreate-dirs
        (make-directory dir t)))
    (let* ((tmp (make-temp-file "weave-" nil ".tmp")))
      (with-temp-file tmp
        (setq buffer-file-coding-system 'utf-8-unix)
        (insert content))
      (rename-file tmp abs t))))

(defun weave-fs-hash (content)
  "Return SHA256 of CONTENT string."
  (secure-hash 'sha256 content))

(defun weave-fs-backup-path (root rel)
  "Compute backup path for REL under ROOT or temp dir."
  (let* ((base (or weave-backup-dir (make-temp-file "weave-bak-" t)))
         (dst  (expand-file-name (format "%s.%s.bak"
                                         (subst-char-in-string ?/ ?_ rel)
                                         (float-time)) base)))
    (make-directory (file-name-directory dst) t)
    dst))

(provide 'weave-fs)
;;; weave-fs.el ends here
