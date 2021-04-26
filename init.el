;;;; init.el ---- -*- lexical-binding: t; -*-

;; Emacs Initialization File

;;; Commentary:

;; Copyright (c) 2016-2018 Yang Chuang
;;
;; Author: Yang Chuang <ifrenzyc@gmail.com>
;; URL: https://github.com/ifrenzyc
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Code:

;; Garbage collection
;; Set garbage collect high to speed up startup
(let ((gc-cons-threshold most-positive-fixnum)
      (ad-redefinition-action 'accept)))     ; Ignore advice warnings

;;;;;;;;;;;;;;
;; Straight ;;
;;;;;;;;;;;;;;
;; Straight.el is a functional package manager for Emacs. It server as
;; a replacement for the native package.el

;; Bootstrap the package manager, straight.el.
;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 5))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

;; (defun straight-reload-init ()
;;   "Reload init.el."
;;   (interactive)
;;   (straight-transaction
;;    (straight-mark-transaction-as-init)
;;    (message "Reloading init.el...")
;;    (load user-init-file nil 'nomessage)
;;    (message "Reloading init.el... done.")))

;; Should be placed inside init.el before anything loading org-mode 
;; https://github.com/yantar92/org
;; (straight-use-package '(org :host github :repo "yantar92/org" :branch "feature/org-fold-universal-core"
;; 			    :files (:defaults "contrib/lisp/*.el")))

;; Added by package.el. This must come before configurations of
;; installed packages. Don't delete this line. If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; uncomment this line in first time startup.
(require 'package)
(unless package--initialized (package-initialize))

(when (>= emacs-major-version 26)
  (require 'package)

  (setq package-archives
        '(("elpy"         . "https://jorgenschaefer.github.io/packages/")
          ("gnu"          . "https://elpa.gnu.org/packages/")
          ;; ("gnu"          . "https://elpa.emacs-china.org/gnu/")
          ("melpa"        . "https://melpa.org/packages/")
          ;; ("melpa"        . "https://elpa.emacs-china.org/melpa/")
          ("org"          . "https://orgmode.org/elpa/")
          ;; ("org"          . "https://elpa.emacs-china.org/org/")
          ))
  )

;; avoid problems with files newer than their byte-compiled counterparts
;; it's better a lower startup than load an outdated and maybe bugged package
(eval-and-compile
  (setq load-prefer-newer t
        package-user-dir "~/.emacs.d/elpa"
        package--init-file-ensured t
        ))

(unless (file-directory-p package-user-dir)
  (make-directory package-user-dir t))

;; initialize the packages and create the packages list if not exists
;; (when (not package-archive-contents)
;;  (package-refresh-contents))

;; install use-package if not exists
;; Bootstrap `use-package'
;; 更新本地仓库里面的 package
;; 参考使用 https://github.com/raxod502/straight.el 扩展 use-package
(setq package-pinned-packages '((use-package . "melpa")))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'bind-key)
  ;; (package-install 'diminish)
  (package-install 'use-package))

;; Call straight-use-package to bootstrap use-package so we can use it.
;; (straight-use-package 'use-package)

;; Should set before loading `use-package'
(eval-when-compile
  (setq use-package-always-ensure t
        use-package-always-defer t
        use-package-compute-statistics t
        use-package-expand-minimally t
        use-package-enable-imenu-support t
        use-package-verbose t))

(eval-when-compile
  (require 'use-package))

(use-package bind-key :demand t)                ;; if you use any :bind variant
;; (use-package diminish
;;   :demand t
;;   :config
;;   (diminish 'visual-line-mode "↩️ "))

;; Use latest org before calling babel
(use-package-pin-package 'org "org")
(use-package org :ensure org-plus-contrib)
(require 'org)
(require 'ob-tangle)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (org . t)
   (shell . t)
   (sql . t)
   (plantuml . t)
   (restclient . t)))

(org-babel-load-file (expand-file-name "~/.emacs.d/emacs.org"))

;; Keep emacs Custom-settings in separate file
;; This keeps your init.el neater and you have the option
;; to gitignore your custom.el if you see fit.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

;;; init.el ends here
