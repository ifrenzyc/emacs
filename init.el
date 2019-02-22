;; -*- lexical-binding: t -*-

;;; init.el --- Emacs Initialization File

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
(require 'package)

;; Added by package.el. This must come before configurations of
;; installed packages. Don't delete this line. If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; uncomment this line in first time startup.
(package-initialize)

(when (>= emacs-major-version 26)
  (require 'package)

  ;; (setq url-proxy-services
  ;; 	  '(("no_proxy" . "^\\(localhost\\|10.*\\)")
  ;; 		("http" . "127.0.0.1:1087")
  ;; 		("https" . "127.0.0.1:1087")))

  ;; (setq url-using-proxy t)
  ;; (setq package-archives '(("gnu"          . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
  ;;                          ("melpa"        . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
  ;;                          ("org"          . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

  ;; (setq package-archives '(("gnu"          . "https://elpa.emacs-china.org/elpa/gnu/")
  ;;                          ("melpa"        . "https://elpa.emacs-china.org/elpa/melpa/")
  ;;                          ("org"          . "https://elpa.emacs-china.org/elpa/org/")))

  (setq package-archives
        '(("elpy"         . "https://jorgenschaefer.github.io/packages/")
          ("gnu"          . "https://elpa.gnu.org/packages/")
          ("melpa"        . "https://melpa.org/packages/")
          ("org"          . "https://orgmode.org/elpa/")))
  )

;; avoid problems with files newer than their byte-compiled counterparts
;; it's better a lower startup than load an outdated and maybe bugged package
(eval-and-compile
  (setq package-enable-at-startup nil
        load-prefer-newer t
        package-user-dir "~/.emacs.d/elpa"
        package--init-file-ensured t
        use-package-compute-statistics t
        ))

(unless (file-directory-p package-user-dir)
  (make-directory package-user-dir t))

;; initialize the packages and create the packages list if not exists
;; (when (not package-archive-contents)
;;  (package-refresh-contents))

;; install use-package if not exists
;; Bootstrap `use-package'
;; 更新本地仓库里面的 package
(setq package-pinned-packages '((use-package . "melpa")))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'bind-key)
  (package-install 'diminish)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t
        use-package-verbose t))

(use-package bind-key :demand t)                ;; if you use any :bind variant
(use-package diminish
  :demand t
  :config
  (diminish 'visual-line-mode "↩️ "))

;; Use latest org before calling babel
(use-package-pin-package 'org "org")
(use-package org :ensure org-plus-contrib)
(require 'org)
(require 'ob-tangle)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (org . t)
   (shell . t)))

(org-babel-load-file (expand-file-name "~/.emacs.d/emacs.org"))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;;; init.el ends here
