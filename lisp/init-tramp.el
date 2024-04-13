;; init-tramp.el --- Initialize tramp settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; - http://www.kohn.com.cn/wordpress/?p=257
;; - https://emacs-china.org/t/emacs-terminal/3140/14
;; - http://gleek.github.io/blog/2017/04/11/editing-remote-code-with-emacs/
;; 

;;; Code:
(require 'init-const)

(use-package tramp
  :ensure nil
  :hook
  ;; https://github.com/masasam/emacs-counsel-tramp#if-you-want-to-speed-up-tramp
  (counsel-tramp-pre-command . (lambda ()
                                 (projectile-mode 0)))
  (counsel-tramp-quit . (lambda ()
                          (projectile-mode 1)))
  :custom
  ((tramp-default-method "ssh")
   (password-cache-expiry nil)
   (tramp-auto-save-directory (expand-file-name ".cache/tramp-autosave-dir" user-emacs-directory))
   (tramp-backup-directory-alist `(("." . ,(concat yc/cache-dir ".saves_tramp"))))
   (tramp-chunksize 2000))
  :init
  (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash")))

(use-package counsel-tramp
  :after (tramp counsel)
  :bind
  ("C-c s" . counsel-tramp))

;; OBSOLETED
;; (use-package docker-tramp
;;   :after tramp
;;   :custom (docker-tramp-use-names t))

(provide 'init-tramp)
;;; init-tramp.el ends here
