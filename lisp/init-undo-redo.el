;; init-undo-redo.el --- Initialize undo-redo settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; 设置 =evil-undo-system= 参数为 =undo-tree= 时，需要先启用 undo-tree
;; - https://github.com/casouri/vundo
;;

;;; Code

(require 'init-const)

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

(provide 'init-undo-redo)
