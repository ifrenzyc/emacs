;; init-undo-redo.el --- Initialize undo-redo settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; - https://github.com/casouri/vundo
;;

;;; Code:
(require 'init-const)
(require 'init-font)

(use-package vundo
  :bind
  (([remap undo] . vundo)
   :map vundo-mode-map
   ("C-a" . vundo-stem-root)
   ("C-e" . vundo-stem-end)
   ;; These are for horizontal movements.
   ("C-f" . vundo-forward)
   ("C-b" . vundo-backward)
   ;; These are for vertical movements.
   ("C-n" . vundo-next)
   ("C-p" . vundo-previous))
  :custom
  ;; Use compact layout
  (vundo-compact-display t)
  ;; Use pretty Unicode characters
  ;; (vundo-glyph-alist vundo-unicode-symbols)
  (vundo-glyph-alist
   '((selected-node   . ?●)
     (node            . ?○)
     (vertical-stem   . ?│)
     (branch          . ?├)
     (last-branch     . ?╰)
     (horizontal-stem . ?─)))
  :config
  (set-face-attribute 'vundo-default nil :family user/unicode-font))

;;================================================================================
(use-package undo-tree
  :disabled t
  :demand t
  :hook (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-auto-save-history nil
        undo-tree-visualizer-diff t
        undo-tree-enable-undo-in-region nil
        undo-tree-visualizer-timestamps t
        undo-tree-history-directory-alist `(("." . ,(concat yc/cache-dir "/undo/"))))

  ;; HACK: keep the diff window
  (with-no-warnings
    (make-variable-buffer-local 'undo-tree-visualizer-diff)
    (setq-default undo-tree-visualizer-diff t)))

;; Prefer undo-fu
;; (use-package undo-fu
;;   :config
;;   (global-undo-tree-mode -1)
;;   (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
;;   (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo))

;; (use-package undo-fu-session)
;;================================================================================

(provide 'init-undo-redo)
;;; init-undo-redo.el ends here
