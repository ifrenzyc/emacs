;; init-utils.el --- Initialize utils configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:

;; A Collection of Ridiculously Useful eXtensions for Emacs.
;; https://github.com/bbatsov/crux
(use-package crux
  :commands (crux-sudo-edit crux-insert-date crux-reopen-as-root-mode crux-rename-file-and-buffer)
  :config
  (defalias 'cb 'crux-create-scratch-buffer))

(use-package logms
  :config
  (logms-mode t))

;;================================================================================
;; buffer's Imenu
;; - https://github.com/rnkn/side-hustle
;; - https://github.com/bmag/imenu-list
(use-package side-hustle
  :disabled t)

;; Restart Emacs inside Emacs.
;; https://github.com/iqbalansari/restart-emacs
(use-package restart-emacs
  :disabled t
  :commands (restart-emacs)
  :config (emacs-restore-frames t))
;;================================================================================

(provide 'init-utils)
;;; init-utils.el ends here
