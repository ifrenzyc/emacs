;; init-theme.el --- Initialize theme settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:

(use-package modus-themes
  :demand t
  :init
  (setq modus-themes-common-palette-overrides
        '((fg-heading-2 fg-main)))

  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-org-blocks 'gray-background)
  :config
  (modus-themes-select 'modus-operandi-tinted))

;;================================================================================
(use-package doom-themes
  :disabled t
  :demand t
  ;; :init
  ;; Global settings (defaults)
  ;; (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
  ;;       doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each
  ;; theme may have their own settings.
  ;; (load-theme 'doom-one t)
  ;; (load-theme 'doom-one-light t)
  ;; (load-theme 'doom-opera-light t)
  ;; (load-theme 'doom-solarized-light t)
  ;; (load-theme 'doom-nord t)
  ;; (load-theme 'doom-nord-light t)
  ;; (load-theme 'doom-spacegrey t)
  ;; (load-theme 'doom-wilmersdorf t)
  ;; (load-theme 'doom-monokai-octagon t)
  ;; (load-theme 'doom-zenburn t)
  ;; :custom
  ;; (doom-themes-treemacs-theme "doom-colors")
  :config
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme
  ;; (doom-themes-neotree-config)    ; all-the-icons fonts must be installed!

  ;; (doom-themes-treemacs-config)
  ;; (with-eval-after-load 'lsp-treemacs
  ;;   (doom-themes-treemacs-config)    ; all-the-icons fonts must be installed!
  ;;   )

  ;; Corrects (and improves) org-mode's native fontification.
  ;; (doom-themes-org-config)
  )

(use-package ef-themes
  :disabled t
  :demand t
  :config
  (ef-themes-select 'ef-eagle))

;; (use-package tron-legacy-theme
;;   :custom
;;   (tron-legacy-theme-vivid-cursor t)
;;   (tron-legacy-theme-softer-bg t)
;;   :config
;;   (load-theme 'tron-legacy t))

;; (use-package cyberpunk-theme
;;   :demand t
;;   :config
;;   (load-theme 'cyberpunk t))

;; (use-package ample-theme
;;   :init (progn (load-theme 'ample t t)
;;                (load-theme 'ample-flat t t)
;;                (load-theme 'ample-light t t)
;;                (enable-theme 'ample-flat)))

;; (use-package kaolin-themes
;;   :demand t
;;   :config
;;   (load-theme 'kaolin-dark t)
;;   (kaolin-treemacs-theme))

;; (use-package zenburn-theme
;;   :demand t
;;   :config
;;   (load-theme 'zenburn t))

;; (use-package anti-zenburn-theme
;;   :demand t
;;   :config
;;   (load-theme 'anti-zenburn t))

(use-package vscode-dark-plus-theme
  ;; :after solaire-mode
  :disabled t
  :demand t
  ;; :straight (:host github :repo "ianyepan/vscode-dark-plus-emacs-theme")
  :custom
  (vscode-dark-plus-scale-org-faces nil)
  (vscode-dark-plus-box-org-todo nil)
  :config
  (load-theme 'vscode-dark-plus t))
;;================================================================================

(provide 'init-theme)
;;; init-theme.el ends here
