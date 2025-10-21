;;; weave-prompts.el --- Canonical prompts for Weave LLM interactions  -*- lexical-binding: t; -*-

;;; Commentary:
;; Provide short, strict prompts for PATCH SIMPLE v1 generation and Q/A.
;; Keep wording minimal and deterministic to improve LLM reliability.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup weave-prompts nil
  "Canonical prompts for Weave."
  :group 'weave)

(defun weave-prompts--sanitize-context (s)
  "Trim and cap context S for prompts."
  (when (and s (stringp s))
    (let* ((trim (string-trim s))
           (max 2000))
      (if (> (length trim) max)
          (substring trim 0 max)
        trim))))

(cl-defun weave-prompts-patch-simple (&key context id title)
  "Return a canonical prompt asking for a PATCH SIMPLE v1 block.
Optional CONTEXT (string) is included verbatim in a fenced text block.
Optional ID and TITLE are hints; the model must still output them in the block."
  (let* ((ctx (weave-prompts--sanitize-context context))
         (id-line (when id (format "id: %s\n" id)))
         (title-line (when title (format "title: %s\n" title))))
    (concat
     "You are an Emacs/Org assistant. Output exactly one org block in the format below.\n"
     "No commentary before or after. Literal matches only. Use the first occurrence.\n\n"
     (when ctx
       (concat "Context (do not echo outside the block):\n"
               "#+begin_src text\n" ctx "\n#+end_src\n\n"))
     "Format to output:\n"
     "#+begin_llm_patch\n"
     "PATCH SIMPLE v1\n"
     (or id-line "id: <slug-or-uuid>\n")
     (or title-line "title: <optional>\n")
     "\n"
     "file: <path/to/file>\n"
     "op: <write|delete|edit>\n"
     "  - For write: include CONTENT section\n"
     "  - For edit: include one or more FIND/WITH pairs\n"
     "\n"
     "---CONTENT--- (for write)\n"
     "... full file contents ...\n"
     "---END CONTENT---\n"
     "\n"
     "---FIND--- (for edit)\n"
     "... exact text to match ...\n"
     "---END FIND---\n"
     "---WITH---\n"
     "... replacement text (may include FIND at start/end for insert) ...\n"
     "---END WITH---\n"
     "#+end_llm_patch")))

(cl-defun weave-prompts-question (&key context)
  "Return a concise prompt to ask a project question.
Optional CONTEXT is inserted as a text block the model can use."
  (let ((ctx (weave-prompts--sanitize-context context)))
    (concat
     "Answer briefly and precisely about the project. Use bullets when helpful.\n"
     "If uncertain, say so and suggest how to verify.\n\n"
     (when ctx
       (concat "Context:\n#+begin_src text\n" ctx "\n#+end_src\n")))))

(provide 'weave-prompts)
;;; weave-prompts.el ends here
