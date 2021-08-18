;; init-ido.el --- Initialize ido settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; - fido-mode: https://www.reddit.com/r/emacs/comments/iaqlpk/fidomode_in_emacs_27/
;; 

;;; Code

(use-package ido
  :straight (:type built-in)
  :demand t
  :init
  (setq ido-use-virtual-buffers t
        ido-use-faces t
        ido-case-fold nil
        ido-auto-merge-work-directories-length -1
        ido-max-prospects 10
        ido-enable-flex-matching t
        ido-use-filename-at-point 'guess
        ido-handle-duplicate-virtual-buffers 2
        ido-create-new-buffer 'always)
  :general
  ("C-x C-f" 'ido-find-file)
  :bind (:map ido-common-completion-map
              ("M-e" . ido-edit-input)
              ("M-r" . ido-toggle-regexp))
  :config
  (ido-vertical-mode 1)
  (ido-everywhere 1)
  (ido-mode t)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package ido-vertical-mode :after ido)

;; Improves sorting for fuzzy-matched results
(use-package flx
  :after ido
  :init
  (setq ivy-flx-limit 10000))

(use-package flx-ido
  :after ido
  :config
  (flx-ido-mode 1)
  ;; disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching t))

(use-package ido-completing-read+
  :after ido
  :config
  (ido-ubiquitous-mode 1))

(provide 'init-ido)
