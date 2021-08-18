;; init-smex.el --- Initialize smex configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:

(use-package smex
  :demand t
  :commands smex
  :config
  (smex-initialize)
  :general
  (;; "M-x" 'smex
   "M-X" 'smex-major-mode-commands))
;; "C-c C-c M-x" 'execute-extended-command  ;; This is your old M-x.

;; - https://github.com/DarwinAwardWinner/amx
;; (use-package amx
;;   :after ivy
;;   :custom
;;   (amx-backend 'auto)
;;   (amx-save-file (expand-file-name "amx-items" user-emacs-directory))
;;   (amx-history-length 50)
;;   (amx-show-key-bindings nil)
;;   :config
;;   (amx-mode 1))

(provide 'init-smex)
