;; init-mini-modeline.el --- Initialize mini-modeline settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; Display mode line information in minibuffer.
;;
;; - https://github.com/kiennq/emacs-mini-modeline/blob/master/README.md
;; - https://github.com/manateelazycat/awesome-tray
;; - https://github.com/tautologyclub/feebleline
;;

;;; Code

(use-package mini-modeline
  ;; :straight (:host github :repo "kiennq/emacs-mini-modeline")
  :demand t
  :config
  (mini-modeline-mode t))

(use-package feebleline
  :disabled t
  :hook
  (after-init . feebleline-mode)
  :config
  (setq feebleline-msg-functions
        '((feebleline-line-number         :post "" :fmt "%5s")
          (feebleline-column-number       :pre ":" :fmt "%-2s")
          (feebleline-file-directory      :face feebleline-dir-face :post "")
          (feebleline-file-or-buffer-name :face font-lock-keyword-face :post "")
          (feebleline-file-modified-star  :face font-lock-warning-face :post "")
          (feebleline-git-branch          :face feebleline-git-face :pre " : ")
          (feebleline-project-name        :align right)))
  (window-divider-mode t)
  (feebleline-mode 1))

(use-package awesome-tray
  :disabled t
  :load-path "localelpa/awesome-tray"
  :demand t
  :hook (after-init . awesome-tray-mode))

(provide 'init-mini-modeline)
