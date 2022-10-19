;; init-snails.el --- Initialize snails configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:

(use-package snails
  :load-path "localelpa/snails"
  :commands snails
  :custom-face
  (snails-content-buffer-face ((t (:background "#111111" :height 120))))
  (snails-input-buffer-face ((t (:background "#999999" :foreground "gold" :height 120))))
  (snails-header-line-face ((t (:inherit font-lock-function-name-face :underline t :height 1.1))))
  :custom
  (snails-show-with-frame nil)
  ;; :hook
  ;; (snails-mode . (lambda () (evil-emacs-state)))
  :config
  ;; Functions for specific backends
  (defun snails-current-project ()
    (interactive)
    (snails '(snails-backend-projectile snails-backend-rg snails-backend-fd)))
  (defun snails-active-recent-buffers ()
    (interactive)
    (snails '(snails-backend-buffer snails-backend-recentf)))
  (defun snails-everywhere ()
    (interactive)
    (snails '(snails-backend-everything snails-backend-mdfind)))
  :bind
  (("M-s p" . snails-current-project)
   ("M-s b" . snails-active-recent-buffers)
   ("M-s e" . snails-everywhere)
   ("M-p" . (lambda ()
              (interactive)
              (snails '(
                        snails-backend-buffer
                        snails-backend-recentf
                        snails-backend-projectile
                        snails-backend-mdfind
                        snails-backend-rg
                        snails-backend-imenu))))
   :map snails-mode-map ([remap next-line] . snails-select-next-item)))

(provide 'init-snails)
