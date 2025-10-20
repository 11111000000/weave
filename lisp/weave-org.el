;;; weave-org.el --- Org helpers for Weave  -*- lexical-binding: t; -*-

;;; Commentary:
;; Find llm_patch blocks, display diagnostics, annotate applied.

;;; Code:

(require 'org)
(require 'org-element)
(require 'cl-lib)
(require 'weave-core)

(defface weave-diag-error-face
  '((t :underline (:style wave :color "red")))
  "Face for error overlays."
  :group 'weave)

(defface weave-diag-warn-face
  '((t :underline (:style wave :color "orange")))
  "Face for warning overlays."
  :group 'weave)

(defvar-local weave--diag-overlays nil
  "Overlays for diagnostics in current buffer.")

(defun weave-org-patch-bounds-at-point ()
  "Return (beg . end) if point is inside a llm_patch special block."
  (org-with-wide-buffer
   (let* ((ctx (org-element-context)))
     (when (and (eq (car ctx) 'special-block)
                (string= (org-element-property :type ctx) "llm_patch"))
       (cons (org-element-property :contents-begin ctx)
             (org-element-property :contents-end ctx))))))

(defun weave-org-all-patch-bounds ()
  "Return list of bounds for all llm_patch blocks in buffer."
  (let (out)
    (org-with-wide-buffer
     (org-element-map (org-element-parse-buffer) 'special-block
       (lambda (el)
         (when (string= (org-element-property :type el) "llm_patch")
           (push (cons (org-element-property :contents-begin el)
                       (org-element-property :contents-end el))
                 out)))))
    (nreverse out)))

(defun weave-org-clear-overlays ()
  "Clear previous diagnostic overlays."
  (mapc #'delete-overlay weave--diag-overlays)
  (setq weave--diag-overlays nil))

(defun weave-org-display-diagnostics (beg end diags)
  "Display DIAGS overlays between BEG and END."
  (weave-org-clear-overlays)
  (dolist (d diags)
    (let* ((face (if (eq (alist-get :severity d) 'error)
                     'weave-diag-error-face 'weave-diag-warn-face))
           (ov (make-overlay beg end)))
      (overlay-put ov 'face face)
      (overlay-put ov 'help-echo (alist-get :message d))
      (push ov weave--diag-overlays))))

(defun weave-org-annotate-applied (beg end id)
  "Insert PROPERTIES drawer with applied metadata after block [BEG,END]."
  (save-excursion
    (goto-char end)
    (end-of-line)
    (insert (format "\n:PROPERTIES:\n:WEAVE-ID: %s\n:WEAVE-APPLIED: %s\n:END:\n"
                    id (format-time-string "%FT%T%z")))))

(provide 'weave-org)
;;; weave-org.el ends here
