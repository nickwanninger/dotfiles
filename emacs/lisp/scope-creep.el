;;; scope-creep.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq scope-creep-selection nil)
(make-variable-buffer-local 'scope-creep-selection)



(defun scope-creep--node-at-point ()
  (condition-case nil
      (treesit-node-at (point))
      ((debug error) nil)))

;; Use `point` to validate the current selection context. This means
;; we ensure that either the `scope-creep-selection` is valid for the
;; node under point, or we need to re-initialize it to the node-under-point
(defun scope-creep--ensure-selection ()
  (let ((node (treesit-node-at (point))))
    (when node
        ;; Make sure the selection has at least one node
        (when (not (and scope-creep-selection
                        (equal (car scope-creep-selection) node)))
            (message "Node is different")
            (setq scope-creep-selection (list node))
            t)
        nil)))


(defun scope-creep--update-selection (node)
  (goto-char (treesit-node-start node))
  (set-mark (treesit-node-end node)))




(defun scope-creep--dump (msg)
  (message "[SC] %s: %s-%s %s" msg (point) (mark) scope-creep-selection))

(defun scope-creep-init-selection ()
  "Begin a treesitter selection"
  (interactive)

  ;; First, reset the selection to nil
  (setq scope-creep-selection nil)

  ;; Then, attempt to grab a node under the (point)
  (let ((node (scope-creep--node-at-point)))
    (when node
      ;; If there was a node at the point, update the selection list
      (setq scope-creep-selection (list node))
      ;; And update the selection visually
      (scope-creep--update-selection node))))



;; When expanding, we need to test a few things:
;; 1. If there is not a current selection, initialize the selection
;; 2. If there is a selection, and the current maek+point region is different, reset the selection to nil and try again
;; 3. if there is a selection, and the current mark+point region is equal, select the parent of the node.

(defun scope-creep-expand-selection ()
  "Start or expand a treesitter a selection"
  (interactive)

  (if (or (not scope-creep-selection)
          (treesit-node-check (car scope-creep-selection)
                              'outdated))
    ;; Rule 1. If the selection is nil, initialize
    (scope-creep-init-selection)
    ;; otherwise, make sure it makes sense and expand it
    (let ((cur (car scope-creep-selection)))
      ;; Rule 2. If the current node is not selected, initialize
      (if (not (and (equal (point) (treesit-node-start cur))
                    (equal (mark) (treesit-node-end cur))))
          ;; ... initialize
          (scope-creep-init-selection)
        ;; Rule 3. Select the parent of the node
        (let ((parent (treesit-node-parent cur)))
          (setq scope-creep-selection (cons parent scope-creep-selection))
          (scope-creep--update-selection parent))))))



(defun scope-creep-contract-selection ()
  "Contract a treesitter a selection"
  (interactive)
  ;; When there is currently a selection...
  (when scope-creep-selection
    ;; Check it is still valid
    (let ((cur (car scope-creep-selection)))
      ;; Only contract if the current node is selected
      (when (and (equal (point) (treesit-node-start cur))
                 (equal (mark) (treesit-node-end cur)))
        ;; Pop off the stack
        (setq scope-creep-selection (cdr scope-creep-selection))
        (when scope-creep-selection
           (scope-creep--update-selection (car scope-creep-selection)))))))



(provide 'scope-creep)
;;; scope-creep.el ends here
