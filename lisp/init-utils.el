;; init-utils.el --- Initialize utils configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:

;; A Collection of Ridiculously Useful eXtensions for Emacs.
;; https://github.com/bbatsov/crux
(use-package crux
  :commands (crux-sudo-edit crux-insert-date crux-reopen-as-root-mode crux-rename-file-and-buffer))

;; buffer's Imenu
;; - https://github.com/rnkn/side-hustle
;; - https://github.com/bmag/imenu-list
(use-package side-hustle
  :straight (:host github :repo "rnkn/side-hustle"))

;; Restart Emacs inside Emacs.
;; https://github.com/iqbalansari/restart-emacs
(use-package restart-emacs
  :commands (restart-emacs)
  :config (emacs-restore-frames t))

(use-package logms
  :config
  (logms-mode t))

(provide 'init-utils)
