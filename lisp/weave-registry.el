;;; weave-registry.el --- Registries for parsers and engines  -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal registries to keep core extensible.

;;; Code:

(require 'cl-lib)
(require 'map)

(defvar weave--parser-registry (make-hash-table :test 'equal)
  "Map signature string to parser function.")

(defun weave-register-parser (signature fn)
  "Register parser FN for SIGNATURE string."
  (puthash signature fn weave--parser-registry))

(defun weave-dispatch-parser (signature)
  "Return parser function registered for SIGNATURE or nil."
  (gethash signature weave--parser-registry))

(provide 'weave-registry)
;;; weave-registry.el ends here
