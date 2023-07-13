;; init-image.el --- Initialize image settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:
(use-package image+
  :init
  (eval-after-load 'image '(require 'image+))
  ;; :hook
  ;; (image-mode . (lambda () (require 'image+)))
  :config
  (imagex-global-sticky-mode t)
  :mode-hydra
  ;; 相关快捷键参考官方文档：https://github.com/mhayashi1120/Emacs-imagex
  (image-mode
   (:hint nil :foreign-keys warn :quit-key "C-g" :title (with-faicon "nf-fa-camera" "Manipulating Image" 1 -0.05) :separator "═")
   ("Resize"
    (("=" imagex-sticky-zoom-in "zoom in")
     ("-" imagex-sticky-zoom-out "zoom out")
     ("w" image-transform-fit-to-width "fit window width")
     ("+" imagex-sticky-maximize "maximize")
     ("0" imagex-sticky-restore-original "reset"))
    "Rotate"
    (("r" imagex-sticky-rotate-right "rotate right")
     ("l" imagex-sticky-rotate-left "rotate left"))
    "Save"
    (("S" imagex-sticky-save-image "save image"))
    ))
  )

(use-package iscroll
  :disabled t
  :hook (org-mode . iscroll-mode)
  :config
  (setq iscroll-preserve-screen-position t))

(provide 'init-image)
;;; init-image.el ends here
