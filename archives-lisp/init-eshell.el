;; init-eshell.el --- Initialize eshell configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; - http://www.howardism.org/Technical/Emacs/eshell-present.html
;; 

;;; Code

(use-package eshell
  :ensure nil
  :config
  (defun yc/eshell-prompt-function ()
    "My eshell prompt function."
    (concat " λ "))

  (setq eshell-highlight-prompt nil
        eshell-hist-ignoredups t
        eshell-directory-name (expand-file-name "eshell" yc/cache-dir)
        eshell-prefer-lisp-functions t
        eshell-prompt-function #'yc/eshell-prompt-function))

(use-package eshell-info-banner
  ;; :straight (:host github :repo "Phundrak/eshell-info-banner.el" :branch main)
  :hook (eshell-banner-load . eshell-info-banner-update-banner))

(use-package aweshell
  :load-path "localelpa/aweshell"
  :commands (aweshell-new aweshell-next aweshell-prev aweshell-toggle)
  :config
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-dakrone))

;; https://github.com/akreisher/eshell-syntax-highlighting/

(provide 'init-eshell)
