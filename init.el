;;;; init.el ---- -*- lexical-binding: t; -*-

;; Emacs Initialization File

;;; Commentary:

;; Copyright (c) 2016-2022 Yang Chuang
;;
;; Author: Yang Chuang <ifrenzyc@gmail.com>
;; URL: https://github.com/ifrenzyc
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3
;;
;; - 关于 use-package 的使用：https://github.com/jwiegley/use-package
;;

;;; Code:

(let ((ad-redefinition-action 'accept)))     ; Ignore advice warnings

;; Added by package.el. This must come before configurations of
;; installed packages. Don't delete this line. If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; uncomment this line in first time startup.
(require 'package)
(unless package--initialized (package-initialize))

(when (>= emacs-major-version 26)
  (setq package-archives
        '(("elpy"         . "https://jorgenschaefer.github.io/packages/")
          ("gnu"          . "https://elpa.gnu.org/packages/")
          ("melpa"        . "https://melpa.org/packages/")
          ("org"          . "https://orgmode.org/elpa/"))))

;; avoid problems with files newer than their byte-compiled counterparts
;; it's better a lower startup than load an outdated and maybe bugged package
(eval-and-compile
  (setq load-prefer-newer t
        package-user-dir (expand-file-name "elpa" user-emacs-directory)
        package--init-file-ensured t))

(unless (file-directory-p package-user-dir)
  (make-directory package-user-dir t))

;; initialize the packages and create the packages list if not exists
(when (not package-archive-contents)
  (package-refresh-contents))

;; install use-package if not exists
;; Bootstrap `use-package'
;; 更新本地仓库里面的 package
;; 参考使用 https://github.com/raxod502/straight.el 扩展 use-package
;; use-package 关键字的说明：
;;   - https://jwiegley.github.io/use-package/keywords/#defer-demand
;;   - https://phenix3443.github.io/notebook/emacs/modes/use-package-manual.html
(setq package-pinned-packages '((use-package . "melpa")))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t
        use-package-always-defer t
        use-package-compute-statistics t
        use-package-expand-minimally t
        use-package-enable-imenu-support t
        use-package-verbose t))

(use-package bind-key)
(use-package diminish)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-const)
(require 'init-basic)
(require 'init-custom)
(require 'init-font)
;; (require 'init-cnfonts)
(require 'init-funcs)
;; (require 'init-package)

(require 'init-macos)   ;; macOS

(require 'init-ui)
(require 'init-theme)
(require 'init-modeline)
;; (require 'init-nano-modeline)
;; (require 'init-mini-modeline)

;; (require 'init-undo-redo)

(require 'init-bookmark)
(require 'init-dashboard)
(require 'init-scratch)

(require 'init-highlight)

(require 'init-which-key)
(require 'init-general)
;; (require 'init-evil)
;; (require 'init-xah-fly-keys)
;; (require 'init-meow)
(require 'init-hydra)

(require 'init-kill-ring)
(require 'init-edit)
(require 'init-diff)

(require 'init-ivy)
;; (require 'init-vertico)
(require 'init-embark)
;; (require 'init-ido)
;; (require 'init-icomplete)
;; (require 'init-helm) ;; I don't use helm
(require 'init-buffer)
(require 'init-company)
;; (require 'init-corfu)
(require 'init-yasnippet)
;; (require 'init-tabnine)
(require 'init-flycheck)
(require 'init-folding)

;; (require 'init-snails)
(require 'init-search)

(require 'init-dired)
;; (require 'init-smex)
(require 'init-utils)

(require 'init-org)
(require 'init-org-agenda)
(require 'init-org-journal)
(require 'init-org-roam)
;; (require 'init-svg-tag)
(require 'init-deft)
(require 'init-markdown)
(require 'init-reader)

(require 'init-window)
(require 'init-projectile)
;; (require 'init-persp)
;; (require 'init-tab-bar)

(require 'init-vcs)

(require 'init-treemacs)
;; (require 'init-neotree)
(require 'init-helpful)

(require 'init-term)
(require 'init-vterm)
;; (require 'init-eshell)  ;; I don't use eshell
(require 'init-rime)    ;; Chinese input method
;; (require 'init-pyim)
(require 'init-tramp)

;; Tools
;; (require 'init-encrypt)
;; (require 'init-anki)
;; (require 'init-mail)
(require 'init-browser)
;; (require 'init-elfeed)
;; (require 'init-irc)
;; (require 'init-reddit-client)
;; (require 'init-pdf)
;; (require 'init-nov)
(require 'init-image)
;; (require 'init-emojis)
(require 'init-plantuml)
(require 'init-pandoc)
;; (require 'init-spell)
(require 'init-youdao-dictionary)
;; (require 'init-blog)
;; (require 'init-dokuwiki)

;; Lang
(require 'lang-basic)
(require 'init-lsp)
;; (require 'init-lsp-bridge)
;; (require 'init-eglot)
;; (require 'init-nox)
(require 'lang-java)
(require 'lang-golang)
(require 'lang-python)
;; (require 'lang-rust)
;; (require 'lang-clojure)
(require 'lang-web)
(require 'lang-vue)
(require 'lang-groovy)
(require 'lang-elisp)
(require 'lang-sql)
(require 'lang-json)
(require 'lang-xml)
(require 'lang-yaml)
(require 'lang-bash)
(require 'lang-dockerfile)
(require 'lang-docker)
(require 'lang-k8s)

(require 'lang-ansible)
(require 'lang-systemd)
(require 'lang-logstash)
(require 'lang-makefile)
(require 'lang-jenkinsfile)
(require 'lang-vimrc)
(require 'lang-restclient)

(require 'init-gc)

;; Keep emacs Custom-settings in separate file
;; This keeps your init.el neater and you have the option
;; to gitignore your custom.el if you see fit.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

;; Start server
(use-package server
  :ensure nil
  :hook
  (after-init . server-mode))

;;; init.el ends here
