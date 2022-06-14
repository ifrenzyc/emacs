;; init-ui.el --- Initialize ui settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; A utility package to collect various Icon Fonts and propertize them within Emacs.
;; - https://github.com/domtronn/all-the-icons.el
;; 
;; M-x all-the-icons-install-fonts
;; 

;;; Code
(use-package whitespace
  :hook
  (prog-mode . whitespace-mode)
  ;; (org-mode . whitespace-mode)
  :custom
  (whitespace-line-column 119)
  (whitespace-style '(face lines-tail)))

;; TODO: automatically add spacing around operators
;; - https://github.com/davidshepherd7/electric-operator

(use-package ws-butler
  :delight ws-butler-mode)

;; 像 vi 一样，在文件的末尾空白行前面显示 =~= 号。
;; - https://github.com/syohex/vi-tilde-fringe
(use-package vi-tilde-fringe
  :init (global-vi-tilde-fringe-mode)
  :delight (vi-tilde-fringe-mode))

(use-package page-break-lines
  :commands page-break-lines-mode
  :init
  (autoload 'turn-on-page-break-lines-mode "page-break-lines")
  :config
  (setq page-break-lines-max-width fill-column)
  :hook
  (help-mode . page-break-lines-mode))

;; 在编辑器左边显示行号，要先看看内建的 display-line-numbers-mode
;; (display-line-numbers-width-start t)

;; (use-package hlinum
;;   :hook (prog-mode . hlinum-activate))

;; (use-package linum
;;   :hook (prog-mode . linum-mode)
;;   :config
;;   (setq linum-format " %3d ")
;;   (global-linum-mode nil))

;; (use-package linum-relative
;;   ;; :init
;;   ;; https://github.com/coldnew/linum-relative/issues/7
;;   ;; (setq linum-relative-format "%3s ")
;;   ;; display current line instead of 0
;;   ;; (setq linum-relative-current-symbol "")
;;   :hook
;;   (prog-mode . linum-relative-mode)
;;   :config
;;   (setq linum-relative-current-symbol ">>")
;;   (linum-relative-global-mode nil))

;; Show native line numbers if possible, otherwise use `linum'
(if (fboundp 'display-line-numbers-mode)
    (use-package display-line-numbers
      :ensure nil
      :hook (prog-mode . display-line-numbers-mode))
  (use-package linum-off
    :demand
    :defines linum-format
    :hook (after-init . global-linum-mode)
    :custom
    (linum-format "%4d ")
    (display-line-numbers-width-start t)
    :config
    ;; Highlight current line number
    (use-package hlinum
      :defines linum-highlight-in-all-buffersp
      :custom-face (linum-highlight-face ((t (:inherit default :background nil :foreground nil))))
      :hook (global-linum-mode . hlinum-activate)
      :init (setq linum-highlight-in-all-buffersp t))))

(use-package all-the-icons
  :demand t
  :when (display-graphic-p)
  :config
  (add-to-list 'all-the-icons-icon-alist
               '("^Rakefile$" all-the-icons-alltheicon "ruby-alt" :face all-the-icons-red))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.go$" all-the-icons-fileicon "go" :face all-the-icons-blue))
  (add-to-list 'all-the-icons-icon-alist
               '("\\go.mod$" all-the-icons-fileicon "go" :face all-the-icons-dblue))
  (add-to-list 'all-the-icons-icon-alist
               '("\\go.sum$" all-the-icons-fileicon "go" :face all-the-icons-dpurple))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(go-mode all-the-icons-fileicon "go" :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(xwidget-webkit-mode all-the-icons-faicon "chrome" :v-adjust -0.1 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(bongo-playlist-mode all-the-icons-material "queue_music" :height 1.2 :face 'all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(bongo-library-mode all-the-icons-material "library_music" :height 1.1 :face 'all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gnus-group-mode all-the-icons-fileicon "gnu" :face 'all-the-icons-silver))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gnus-summary-mode all-the-icons-octicon "inbox" :height 1.0 :v-adjust 0.0 :face 'all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gnus-article-mode all-the-icons-octicon "mail" :height 1.1 :v-adjust 0.0 :face 'all-the-icons-lblue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(message-mode all-the-icons-octicon "mail" :height 1.1 :v-adjust 0.0 :face 'all-the-icons-lblue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(diff-mode all-the-icons-octicon "git-compare" :v-adjust 0.0 :face all-the-icons-lred))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(flycheck-error-list-mode all-the-icons-octicon "checklist" :height 1.1 :v-adjust 0.0 :face all-the-icons-lred))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.rss$" all-the-icons-octicon "rss" :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(elfeed-search-mode all-the-icons-faicon "rss-square" :v-adjust -0.1 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(elfeed-show-mode all-the-icons-octicon "rss" :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(newsticker-mode all-the-icons-faicon "rss-square" :v-adjust -0.1 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(newsticker-treeview-mode all-the-icons-faicon "rss-square" :v-adjust -0.1 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(newsticker-treeview-list-mode all-the-icons-octicon "rss" :height 1.1 :v-adjust 0.0 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(newsticker-treeview-item-mode all-the-icons-octicon "rss" :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.[bB][iI][nN]$" all-the-icons-octicon "file-binary" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.c?make$" all-the-icons-fileicon "gnu" :face all-the-icons-dorange))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.conf$" all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.toml$" all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(conf-mode all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(conf-space-mode all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(forge-topic-mode all-the-icons-alltheicon "git" :face all-the-icons-blue))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.xpm$" all-the-icons-octicon "file-media" :v-adjust 0.0 :face all-the-icons-dgreen))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(help-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1 :face all-the-icons-purple))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(helpful-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1 :face all-the-icons-purple))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(Info-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1))
  (add-to-list 'all-the-icons-icon-alist
               '("NEWS$" all-the-icons-faicon "newspaper-o" :height 0.9 :v-adjust -0.2))
  (add-to-list 'all-the-icons-icon-alist
               '("Cask\\'" all-the-icons-fileicon "elisp" :height 1.0 :v-adjust -0.2 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(cask-mode all-the-icons-fileicon "elisp" :height 1.0 :v-adjust -0.2 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-icon-alist
               '(".*\\.ipynb\\'" all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebooklist-mode all-the-icons-faicon "book" :face all-the-icons-lorange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebook-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebook-multilang-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-dorange))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.epub\\'" all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(nov-mode all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gfm-mode all-the-icons-octicon "markdown" :face all-the-icons-lblue))
  ;; :init
  ;; 解决 modeline 有一些图标被隐藏的问题
  ;; see https://github.com/hlissner/doom-emacs/blob/develop/modules/ui/modeline/README.org#the-right-side-of-the-modeline-is-cut-off
  ;; (setq all-the-icons-scale-factor 1.1)
  )

(use-package all-the-icons-ivy
  :after (all-the-icons projectile ivy counsel counsel-projectile)
  ;; :demand t
  :hook (after-init . all-the-icons-ivy-setup)
  :config
  (setq all-the-icons-ivy-file-commands
        '(counsel-find-file
          counsel-file-jump
          counsel-recentf
          counsel-projectile-find-file
          counsel-projectile-find-dir
          counsel-projectile)))

(use-package all-the-icons-ivy-rich
  :demand t
  :hook (after-init . all-the-icons-ivy-rich-mode))

(use-package all-the-icons-ibuffer
  :demand t
  :hook (after-init . all-the-icons-ibuffer-mode)
  :init
  ;; Use human readable file size in ibuffer.
  (setq all-the-icons-ibuffer-human-readable-size t))

;; Make certain buffers grossly incandescent
;; https://github.com/hlissner/emacs-solaire-mode
(use-package solaire-mode
  :demand t
  :hook
  ((after-init . solaire-global-mode)
   ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
   ;; (minibuffer-setup . solaire-mode-in-minibuffer)
   )
  :custom
  (solaire-mode-remap-fringe t))

;; very long lines
;; - https://www.reddit.com/r/emacs/comments/ccoksw/solong_mitigating_slowness_due_to_extremely_long/
;; 有时候会打开一些文件，这些文件里的某一行特别长，而 Emacs 没有针对这种情况做特殊 处理，会导致整个界面卡死。现在它来了！
;; 当打开一个具有长行的文件时，它会自动检测并将一些可能导致严重性能的 mode 关闭， 如 font-lock (syntax highlight)。
(use-package so-long
  :ensure nil
  :hook
  (after-init . global-so-long-mode)
  :config
  (setq so-long-threshold 400))

(provide 'init-ui)

