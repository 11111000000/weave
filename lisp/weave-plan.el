;;; weave-plan.el --- Dry-run planning for Weave patches  -*- lexical-binding: t; -*-

;;; Commentary:
;; Build a plan for a patch: check applicability, compute idempotency flags, preview.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'weave-core)
(require 'weave-fs)
(require 'weave-path)

(defun weave-plan--edit-apply-once (text change)
  "Apply CHANGE to TEXT once (first occurrence). Return (values new-text found idempotent)."
  (let* ((find (alist-get :find change))
         (with (alist-get :with change))
         (idx  (and (not (string-empty-p find))
                    (string-match (regexp-quote find) text))))
    (if (not idx)
        (list text nil nil)
      (let* ((end (+ idx (length find)))
             (current (substring text idx end))
             (idempotent (string= current with))
             (new (concat (substring text 0 idx) with (substring text end))))
        (list new t idempotent)))))

(defun weave-plan--file-plan-edit (abs changes)
  "Compute results for editing ABS with CHANGES. Return alist (ops, found, idempotent, new-text)."
  (pcase (weave-fs-read-file abs)
    (`(,text ,mtime ,size)
     (let ((curr text) (ops '()) all-found t)
       (cl-loop for change in changes
                for idx from 0
                do (pcase-let ((`(,next ,found ,idem) (weave-plan--edit-apply-once curr change)))
                     (push `(:op edit :index ,idx :kind ,(alist-get :kind change)
                                 :found ,(and found t) :idempotent ,(and idem t))
                           ops)
                     (setq all-found (and all-found found))
                     (setq curr next)))
       `((ops . ,(nreverse ops))
         (found . ,all-found)
         (idempotent . ,(cl-every (lambda (o) (alist-get :idempotent o)) ops))
         (new-text . ,curr)
         (mtime . ,mtime) (size . ,size))))
    (_ `((ops . nil) (found . nil) (idempotent . nil) (new-text . nil)))))

(defun weave-plan (patch root)
  "Build a dry-run PLAN alist for PATCH relative to ROOT."
  (let ((files '()) (errors '()))
    (dolist (op (alist-get :ops patch))
      (let* ((rel (alist-get :file op))
             (val (weave-path-validate-rel rel root))
             (safe (car val)) (perr (cadr val)))
        (when perr (setq errors (nconc errors perr)))
        (when safe
          (let* ((abs (expand-file-name safe root)))
            (pcase (alist-get :type op)
              ('write
               (let* ((content (or (alist-get :content op) ""))
                      (exists (file-exists-p abs))
                      (size (string-bytes content)))
                 (push `(:file ,safe
                               :ops ((:op write
                                          :will-create ,(not exists)
                                          :will-overwrite ,exists
                                          :size-new ,size))
                               :_write-content ,content)
                       files)))
              ('delete
               (push `(:file ,safe :ops ((:op delete :will-delete ,(file-exists-p abs))))
                     files))
              ('edit
               (let* ((res (weave-plan--file-plan-edit abs (alist-get :changes op))))
                 (when (not (alist-get :found res))
                   (push (weave-core-make-diagnostic
                          :severity 'error :code :not-found
                          :message (format "Anchor not found in %s" safe))
                         errors))
                 (push `(:file ,safe
                               :ops ,(alist-get :ops res)
                               :_new-text ,(alist-get :new-text res)
                               :_mtime ,(alist-get :mtime res)
                               :_size ,(alist-get :size res))
                       files))))
            ))))
    (let* ((ok (and (null errors)
                    (cl-every
                     (lambda (f)
                       (let ((ops (alist-get :ops f)))
                         (or (null ops)
                             (cl-some (lambda (o) (eq (alist-get :op o) 'write))
                                      ops)
                             (cl-some (lambda (o) (memq (alist-get :found o) '(t)))
                                      ops))))
                     files)))
           (plan `(:patch-id ,(alist-get :id patch)
                             :files ,(nreverse files)
                             :ok ,ok
                             :errors ,errors)))
      plan)))

(provide 'weave-plan)
;;; weave-plan.el ends here
