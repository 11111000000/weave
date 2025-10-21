;;; weave-llm.el --- LLM adapter (gptel-backed) for Weave  -*- lexical-binding: t; -*-

;;; Commentary:
;; Thin adapter to request patches/questions. Keeps coupling to gptel minimal.

;;; Code:

(require 'cl-lib)
(require 'weave-session)
(require 'weave-prompts)

(declare-function gptel-send "gptel")
(defvar gptel-post-response-functions)

(defun weave-llm-question ()
  "Ask an LLM a question using gptel in current buffer.
Inserts a canonical question prompt, then streams the response."
  (interactive)
  (unless (fboundp 'gptel-send)
    (user-error "gptel not available"))
  (let* ((ctx (when (use-region-p)
                (buffer-substring-no-properties (region-beginning) (region-end))))
         (prompt (weave-prompts-question :context ctx)))
    (insert prompt "\n\n"))
  (gptel-send))

(defun weave-llm-generate-patch ()
  "Request a PATCH SIMPLE v1 from the LLM via gptel and mark generated region.
Inserts the canonical prompt; relies on gptel-post-response-functions to mark the group."
  (interactive)
  (unless (fboundp 'gptel-send)
    (user-error "gptel not available"))
  (weave-session-begin-group)
  (let ((hook (lambda (&rest _)
                (remove-hook 'gptel-post-response-functions hook t)
                (weave-session-finish-group))))
    (add-hook 'gptel-post-response-functions hook nil t))
  (let* ((ctx (when (use-region-p)
                (buffer-substring-no-properties (region-beginning) (region-end))))
         (prompt (weave-prompts-patch-simple :context ctx)))
    (insert prompt "\n\n"))
  (gptel-send))

(provide 'weave-llm)
;;; weave-llm.el ends here
