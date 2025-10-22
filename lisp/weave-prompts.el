;;; weave-prompts.el --- Canonical prompts for Weave LLM interactions  -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides deterministic, composable prompts for PATCH SIMPLE v1, Q/A and future flows.
;; All templates strive for minimalism and strong structure.
;; Extension points: new prompt-kinds (with weave-prompts-register), more context fields.

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
CONTEXT may be an alist or string, see =weave-prompts--compose-context'.
ID (string) and TITLE (string) are optional hints; the model must output them in the block.

Returns a string with a minimal template as concrete example.

Example output (fragment):

    #+begin_llm_patch
    PATCH SIMPLE v1
    id: example-42
    title: Add logging

    file: src/app/main.py
    op: edit
    ---FIND---
    print(\"Started\")
    ---END FIND---
    ---WITH---
    logger.info(\"Started\")
    ---END WITH---
    #+end_llm_patch
"
  (let/ ((ctx-block (weave-prompts--compose-context context))
         (id-line (when id (format "id: %s\n" id)))
         (title-line (when title (format "title: %s\n" title))))
        (concat
         "You are an Emacs/Org Mode coding assistant. Output exactly one org block in the format below.\n"
         "No text before or after, only the patch block. Use literal string matches, first occurrence only.\n\n"
         ctx-block
         "Format example:\n"
         "#+begin_llm_patch\n"
         "PATCH SIMPLE v1\n"
         (or id-line "id: <slug-or-uuid>\n")
         (or title-line "title: <optional>\n")
         "\n"
         "file: <path/to/file>\n"
         "op: <write|delete|edit>\n"
         "---CONTENT---\n<full file content>\n---END CONTENT---\n"
         "---FIND---\n<text to match>\n---END FIND---\n"
         "---WITH---\n<replacement>\n---END WITH---\n"
         "#+end_llm_patch\n")))

(defun weave-prompts--compose-context (ctx)
  "Format context for prompt.
CTX may be string or alist: '((:project . ...) (:selection . ...) ...).
Returns a ready-to-embed string (with Org fencing) or empty."
  (cond
   ((and ctx (stringp ctx))
    (let ((s (string-trim ctx)))
      (if (string-empty-p s) "" (format "Context:\n#+begin_src text\n%s\n#+end_src\n\n" s))))
   ((and ctx (listp ctx))
    (let ((s
           (mapconcat
            (lambda (kv)
              (format "%s: %s" (capitalize (substring (symbol-name (car kv)) 1)) (cdr kv)))
            ctx "\n")))
      (if (string-empty-p s) "" (format "Context:\n#+begin_src text\n%s\n#+end_src\n\n" s))))
   (t "")))

(defun weave-prompts-explain-format ()
  "Return short explanation of PATCH SIMPLE v1 for user help/FAQ."
  (concat
   "Patch format block for weave-mode (PATCH SIMPLE v1):\n"
   "#+begin_llm_patch\nPATCH SIMPLE v1\nid: <slug-or-uuid>\ntitle: <optional>\n\nfile: <relative/path>\nop: <write|delete|edit>\n"
   "---CONTENT--- ... ---END CONTENT--- ; for write\n"
   "---FIND--- ... ---END FIND--- (edit)\n"
   "---WITH--- ... ---END WITH--- (edit)\n# +end_llm_patch\n"
   "Anchoring and insert: for insert-after, start WITH with FIND; for insert-before, end WITH with FIND.\n"))

(defvar weave--prompts-registry (make-hash-table :test 'equal)
  "Registry for extended prompt generators.")

(defun weave-prompts-register (kind fn)
  "Register a prompt generator FN for KIND (symbol)."
  (puthash kind fn weave--prompts-registry))

(defun weave-prompts-get (kind &rest args)
  "Retrieve a prompt by KIND (symbol), passing ARGS to the function."
  (let ((fn (gethash kind weave--prompts-registry)))
    (if fn (apply fn args)
      (error "Prompt kind not registered: %s" kind))))


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
