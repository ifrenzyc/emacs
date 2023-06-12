;; init-image.el --- Initialize image settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; TODO: 这个感觉 image-mode 和 imagex-mode 都没加载到，导致 hydra 按键无法绑定
;; 相关快捷键参考官方文档：https://github.com/mhayashi1120/Emacs-imagex
;; 

;;; Code

(add-hook 'image-mode-hook #'(lambda () (require 'image+)))
(use-package image+
  ;; :after image-mode
  :init (eval-after-load 'image '(require 'image+))
  :config
  (imagex-global-sticky-mode t)
  ;; (when (featurep 'evil)
  ;;   (setq evil-emacs-state-modes (delq 'image-mode evil-emacs-state-modes)))

  ;; (eval-after-load 'image+ '(imagex-global-sticky-mode t))

  ;; TODO 整理可以用 major-mode-hydra 来绑定
  (defvar jp-imagex--title (with-faicon "camera" "Manipulating Image" 1 -0.05))
  (pretty-hydra-define hydra-imagex-sticky
    (:hint nil :foreign-keys warn :quit-key "q" :title jp-imagex--title :separator "═")
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
  ;; :general
  ;; (yc/leader-keys-major-mode
  ;;   :keymaps 'image-mode-map
  ;;   "?"  'hydra-imagex-sticky/body
  ;;   "\\" 'hydra-imagex-sticky/body)
  )
;; ("0" . imagex-sticky-restore-original)
;; ("+" . imagex-sticky-maximize)
;; ("=" . imagex-sticky-zoom-in)
;; ("-" . imagex-sticky-zoom-out)))

(use-package iscroll
  :disabled t
  ;; :straight (:host github :repo "casouri/iscroll")
  :hook (org-mode . iscroll-mode)
  :config
  (setq iscroll-preserve-screen-position t))

;; (eval-after-load 'image+
;;      (bind-keys :map image-mode-map
;;                 ("\\" . hydra-image-sticky/body))
;;      ))
;; (defhydra hydra-imagex-sticky (:color red)
;;   "
;;           Manipulating Image
;;           ^Resize^             ^Rotate^           ^Save             ^Quit
;;           ^^^^^^------------------------------------------------------------
;;           _=_: zoom in     _r_: rotate right         _S_: save file _q_: quit
;;           _-_: zoom out    _l_: rotate left
;;           _+_: maximize
;;           _0_: reset
;;                "
;;   ("\\" hydra-master/body "back")
;;   ("q" nil "quit")
;;   ("=" imagex-sticky-zoom-in)
;;   ("-" imagex-sticky-zoom-out)
;;   ("+" imagex-sticky-maximize)
;;   ("0" imagex-sticky-restore-original)
;;   ("S" imagex-sticky-save-image :color red)
;;   ("r" imagex-sticky-rotate-right)
;;   ("l" imagex-sticky-rotate-left))

(provide 'init-image)
