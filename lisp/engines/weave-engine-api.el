;;; weave-engine-api.el --- Engine protocol (placeholder)  -*- lexical-binding: t; -*-

;;; Commentary:
;; Define a minimal abstract protocol for engines. Future engines implement it.

;;; Code:

(cl-defgeneric weave-engine-question (engine prompt &key callback)
  "Send PROMPT to ENGINE and call CALLBACK with response.")

(cl-defgeneric weave-engine-generate-patch (engine prompt &key callback)
  "Ask ENGINE to generate a patch; CALLBACK receives inserted text or nil.")

(provide 'weave-engine-api)
;;; weave-engine-api.el ends here
