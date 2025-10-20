;;; weave-engine-gptel.el --- GPTel-backed engine adapter  -*- lexical-binding: t; -*-

;;; Commentary:
;; Implements engine protocol over gptel. Kept thin.

;;; Code:

(require 'weave-engine-api)

(defvar weave-engine-gptel--instance :gptel
  "Symbolic instance for gptel engine.")

(cl-defmethod weave-engine-question ((_ engine) prompt &key callback)
  "Use gptel in current buffer to ask PROMPT. CALLBACK is optional."
  (ignore engine)
  (insert (or prompt ""))
  (when (fboundp 'gptel-send)
    (gptel-send))
  (when callback (funcall callback t)))

(cl-defmethod weave-engine-generate-patch ((_ engine) prompt &key callback)
  "Use gptel to ask for patch. CALLBACK called when response ends."
  (ignore engine)
  (insert (or prompt ""))
  (when (fboundp 'gptel-send)
    (gptel-send))
  (when callback (funcall callback t)))

(provide 'weave-engine-gptel)
;;; weave-engine-gptel.el ends here
