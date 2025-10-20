;;; weave-core.el --- Core schemas and helpers for Weave  -*- lexical-binding: t; -*-

;;; Commentary:
;; Stable alist schemas, change classification, and error codes.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'map)
(require 'subr-x)

(defconst weave--format-signature "PATCH SIMPLE v1"
  "Supported patch block signature.")

(defconst weave--max-section-bytes (* 1024 1024)
  "Maximum section payload size in bytes (1MB by default).")

;; Error/diagnostic codes
(defconst weave-error-codes
  '(:bad-signature :missing-id :unknown-op :missing-file :missing-op
                   :missing-section :unmatched-section-tag :bad-path :absolute-path
                   :parent-dir :symlink :empty-find :not-found :apply-drift :io-failure)
  "Domain error codes used by Weave.")

(defconst weave-warn-codes
  '(:unused-metadata :short-find :ignored-by-git :already-applied :noop)
  "Warning codes used by Weave.")

(defun weave-core-classify-change (find with)
  "Classify change kind from FIND and WITH.
Returns one of symbols: replace, insert_before, insert_after, wrap, delete_snippet."
  (cond
   ((string-empty-p with) 'delete_snippet)
   ((string-prefix-p find with) 'insert_after)
   ((string-suffix-p find with) 'insert_before)
   ((string-match-p (regexp-quote find) with) 'wrap)
   (t 'replace)))

(defun weave-core-make-diagnostic (&key severity code message pos line col file section)
  "Construct a diagnostic alist.
SEVERITY is 'error or 'warn."
  `(:severity ,severity :code ,code :message ,message
              :pos ,pos :line ,line :col ,col :file ,file :section ,section))

(provide 'weave-core)
;;; weave-core.el ends here
