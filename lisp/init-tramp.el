;; init-tramp.el --- Initialize tramp settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; - http://www.kohn.com.cn/wordpress/?p=257
;; - https://emacs-china.org/t/emacs-terminal/3140/14
;; - http://gleek.github.io/blog/2017/04/11/editing-remote-code-with-emacs/
;; 

;;; Code
(use-package tramp
  :ensure nil
  :init
  (setq tramp-default-method "ssh")
  (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
  (setq password-cache-expiry nil)
  :config
  (setq tramp-auto-save-directory (expand-file-name ".cache/tramp-autosave-dir" user-emacs-directory)
        tramp-backup-directory-alist `(("." . ,(concat yc/cache-dir ".saves_tramp")))
        tramp-inline-compress-start-size 10000000
        tramp-chunksize 2000
        ;; Make SSH work faster by reusing connections
        tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

  ;; https://github.com/masasam/emacs-counsel-tramp#if-you-want-to-speed-up-tramp
  (add-hook 'counsel-tramp-pre-command-hook #'(lambda ()
                                                (projectile-mode 0)
                                                ))
  (add-hook 'counsel-tramp-quit-hook #'(lambda ()
                                         (projectile-mode 1)
                                         )))

(use-package counsel-tramp
  :after (tramp counsel)
  :general
  ("C-c s" 'counsel-tramp))

;; OBSOLETED
;; (use-package docker-tramp
;;   :after tramp
;;   :custom (docker-tramp-use-names t))

(provide 'init-tramp)
