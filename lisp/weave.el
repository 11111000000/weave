;;; weave.el --- Weave Mode umbrella loader and entry points  -*- lexical-binding: t; -*-

;; Author: AZ
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (org "9.6") (transient "0.7.4"))
;; Keywords: tools, convenience
;; URL: https://example.com/weave

;;; Commentary:
;; Umbrella module that loads Weave's minor mode, commands and thin ports.
;; Users (require 'weave) and M-x weave-mode in Org buffers.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'map)

;; Core and subsystems
(require 'weave-core)
(require 'weave-path)
(require 'weave-fs)
(require 'weave-registry)
(require 'weave-parse)
(require 'weave-block-patch-simple)
(require 'weave-plan)
(require 'weave-apply)

;; UI and integrations
(require 'weave-log)
(require 'weave-org)
(require 'weave-mode)
(require 'weave-transient)

;; Optional LLM adapters (soft-require)
(require 'weave-llm  nil t)

;;;###autoload
(defun weave-version ()
  "Return Weave Mode version as string."
  (interactive)
  (message "Weave Mode 0.0.1")
  "0.0.1")

(provide 'weave)
;;; weave.el ends here
