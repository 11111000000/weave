;;; weave-mode.el --- Minor mode for weaving patches and queries  -*- lexical-binding: t; -*-

;; Author: AZ
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (org "9.6") (transient "0.7.4"))
;; Keywords: tools, convenience

;;; Commentary:
;; Minor mode that adds commands and keybindings on top of Org for Weave.
;; Clean core + thin ports. C-c w opens the menu. C-c C-c applies patch at point.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'weave-org)
(require 'weave-parse)
(require 'weave-plan)
(require 'weave-apply)
(require 'weave-log)

;;; Autoload to avoid circular require with weave-transient
(autoload 'weave-transient-menu "weave-transient" nil t)

(defgroup weave nil
  "Weave Mode customization."
  :group 'tools
  :prefix "weave-")

(defcustom weave-confirm-apply t
  "When non-nil, ask for confirmation before applying a patch."
  :type 'boolean
  :group 'weave)

(defcustom weave-annotate-applied t
  "When non-nil, annotate applied blocks with a PROPERTIES drawer."
  :type 'boolean
  :group 'weave)

;;;###autoload
(define-minor-mode weave-mode
  "Toggle Weave minor mode.
Adds Org helpers and Weave commands in Org buffers."
  :lighter " Weave"
  :keymap (let ((map (make-sparse-keymap)))
            ;; Main menu
            (define-key map (kbd "C-c w") #'weave-transient-menu)
            ;; Context apply
            (define-key map (kbd "C-c C-c") #'weave-ctrl-c-ctrl-c)
            map)
  (if weave-mode
      (unless (derived-mode-p 'org-mode)
        (weave-mode -1)
        (user-error "Weave requires org-mode"))
    (when (derived-mode-p 'org-mode)
      (weave-org-clear-overlays))))

(defun weave-ctrl-c-ctrl-c ()
  "Contextual C-c C-c for Weave.
If on a llm_patch block, dry-run then apply. Otherwise, delegate to Org."
  (interactive)
  (pcase (weave-org-patch-bounds-at-point)
    (`(,beg . ,end)
     (weave-apply-at-point))
    (_ (let ((org-fn (key-binding (kbd "C-c C-c"))))
         (if org-fn (call-interactively org-fn)
           (user-error "Nothing to do here"))))))

;;; High-level commands

;;;###autoload
(defun weave-dry-run-at-point ()
  "Dry-run the llm_patch block at point and show a preview."
  (interactive)
  (pcase (weave-org-patch-bounds-at-point)
    (`(,beg . ,end)
     (pcase-let* ((`(,patch ,diags) (weave-parse-patch-bounds beg end))
                  (root (weave-path-project-root))
                  (plan (and patch (weave-plan patch root))))
       (weave-org-display-diagnostics beg end diags)
       (if patch
           (weave-log-preview plan)
         (user-error "No patch parsed at point"))))
    (_ (user-error "Place point inside a #+begin_llm_patch block"))))

;;;###autoload
(defun weave-apply-at-point ()
  "Apply the llm_patch block at point transactionally."
  (interactive)
  (pcase (weave-org-patch-bounds-at-point)
    (`(,beg . ,end)
     (pcase-let* ((`(,patch ,diags) (weave-parse-patch-bounds beg end))
                  (root (weave-path-project-root)))
       (weave-org-display-diagnostics beg end diags)
       (unless patch (user-error "Patch parse failed"))
       (let ((plan (weave-plan patch root)))
         (unless (alist-get :ok plan) (user-error "Patch is not applicable; see diagnostics/preview"))
         (when (or (not weave-confirm-apply)
                   (weave-log-confirm-apply plan))
           (pcase (weave-apply plan)
             (`(t . ,_)
              (when weave-annotate-applied
                (weave-org-annotate-applied beg end (alist-get :patch-id plan)))
              (message "Weave: applied %s" (alist-get :patch-id plan))))
           (weave-org-clear-overlays)))))
    (_ (user-error "Place point inside a #+begin_llm_patch block"))))

;;;###autoload
(defun weave-apply-all ()
  "Apply all valid llm_patch blocks in the current Org buffer."
  (interactive)
  (let* ((root (weave-path-project-root))
         (blocks (weave-org-all-patch-bounds)))
    (if (null blocks)
        (user-error "No llm_patch blocks in this buffer")
      (let (ok cnt errs)
        (dolist (bounds blocks)
          (pcase-let* ((`(,beg . ,end) bounds)
                       (`(,patch ,diags) (weave-parse-patch-bounds beg end)))
            (weave-org-display-diagnostics beg end diags)
            (when patch
              (let ((plan (weave-plan patch root)))
                (if (alist-get :ok plan)
                    (progn
                      (push (alist-get :patch-id plan) ok)
                      (when (or (not weave-confirm-apply)
                                (weave-log-confirm-apply plan))
                        (weave-apply plan)
                        (setq cnt (1+ (or cnt 0)))))
                  (push (cons (alist-get :patch-id patch) plan) errs))))))
        (message "Weave: applied %d patch(es), %d error(s)"
                 (or cnt 0) (length errs))))))

(provide 'weave-mode)
;;; weave-mode.el ends here
