;;; weave-transient.el --- Transient UI for Weave  -*- lexical-binding: t; -*-

;;; Commentary:
;; Transient menu with core actions.

;;; Code:

(require 'transient)
(require 'weave-mode)

(transient-define-prefix weave-transient-menu ()
  "Weave menu."
  [["LLM"
    ("q" "Ask question" weave-llm-question :if (lambda () (fboundp 'weave-llm-question)))
    ("g" "Generate patch" weave-llm-generate-patch :if (lambda () (fboundp 'weave-llm-generate-patch)))]
   ["Patch"
    ("d" "Dry-run at point"  weave-dry-run-at-point)
    ("a" "Apply at point"    weave-apply-at-point)
    ("A" "Apply all in buffer" weave-apply-all)
    ("L" "Apply last generated" weave-apply-last-generated :if (lambda () (fboundp 'weave-apply-last-generated)))]
   ["Log"
    ("v" "Preview plan" weave-dry-run-at-point)
    ("l" "Show log" (lambda () (interactive) (display-buffer "*weave-log*")))]
   ])

(provide 'weave-transient)
;;; weave-transient.el ends here
