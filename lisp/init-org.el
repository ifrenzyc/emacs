;; init-org.el --- Initialize org-mode configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; | key sequence | function                | 作用说明                                       |
;; |--------------+-------------------------+------------------------------------------------|
;; | =C-x n s=      | ~(org-narrow-to-subtree)~ | 只显示当前光标所在的这一个标题上，其他全部隐藏 |
;; | =C-x n w=      | ~(winden)~                | 显示其他被应用的内容，与 =C-x n s= 正好相反      |
;; | Registers   |                       |                                                                      |
;; | C-x r j     | M-x jump-to-register  | Prompts for register letter. Jumpts to point saved in that register. |
;; | C-x r SPC   | M-x point-to-register | Prompts for register letter. Saves point in register.                |
;; 
;; *orgmode 配置参考：*
;; - https://emacs.lujianmei.com/03-editing/init-org-mode.html
;; - https://emacs.lujianmei.com/03-editing/init-gtd-management.html
;; - https://mstempl.netlify.com/post/beautify-org-mode/
;; 

;;; Code:
(require 'init-font)

(use-package org
  :ensure nil
  :mode ("\\.\\(org\\|txt\\)$" . org-mode)
  :delight org-mode "Org"
  :bind
  (("H-c"     . org-capture)
   ("H-d"     . (lambda ()
                  (interactive)
                  (org-capture nil "d")))
   ([f5]      . yc/copy-idlink-to-clipboard)
   ;; ([f7]      . org-redisplay-inline-images)
   ("M-s M-R" . yc/counsel-rg-in-itsycnotes))
  (:map org-mode-map
        ("C-c C-j"   . counsel-org-goto)
        ("<backtab>" . org-shifttab)
        ("<tab>"     . org-cycle))
  :mode-hydra
  (org-mode
   (:title "Org-mode Commands")
   ("Move"
    (("n" outline-next-visible-heading "next heading" :color pink)
     ("p" outline-previous-visible-heading "prev heading" :color pink)
     ("N" org-forward-heading-same-level "next heading at same level" :color pink)
     ("P" org-backward-heading-same-level "prev heading at same level" :color pink)
     ("u" outline-up-heading "up heading" :color pink)
     ("g" org-goto "goto" exit t))
    "Zoom"
    (("<" worf-back-to-heading "worf-back-to-heading")
     ("R" worf-right "worf-right")
     ("D" worf-down "worf-down")
     ("U" worf-up "worf-up")
     ("L" worf-left "worf-left"))
    "Shift"
    (("K" org-move-subtree-up "up" :color pink)
     ("J" org-move-subtree-down "down" :color pink)
     ("h" org-promote-subtree "promote" :color pink)
     ("l" org-demote-subtree "demote" :color pink))
    "Travel"
    (("b" org-backward-heading-same-level "backward" :color pink)
     ("B" org-forward-heading-same-level "forward" :color pink)
     ("j" hydra-org-child-level "to child" :color pink)
     ("k" hydra-org-parent-level "to parent" :color pink)
     ("a" hydra-org-goto-first-sibling "first sibling")
     ("e" hydra-org-goto-last-sibling "last sibling"))
    "Perform"
    (("r" (lambda () (interactive)
            (call-interactively 'org-cycle)
            (call-interactively 'org-cycle)) "rifle")
     ("v"   avy-org-goto-heading-timer "avy"))
    "Toggles"
    (("C-l" yc/org-toggle-link-display "link")
     ("C-i" org-toggle-inline-images   "image"))
    "Quit"
    (("C-g" nil "quit")
     ("q"   nil "quit"))))
  :custom
  ;; "#0098dd"
  ;; "#0fbf5c"
  ;; "#2c3e50"
  ;; "#2ecc71"
  ;; "#3498db"
  ;; "#50a14f"
  ;; "#7c7c75"
  ;; "#8abeb7"
  ;; "#9f7efe"
  ;; "#b294bb"
  ;; "#b5bd68"
  ;; "#de935f"
  ;; "#e74c3c"
  ;; "#f0c674"
  ;; "#f1c40f"
  ;; "#feb24c"
  ;; "#ff6480"
  (org-todo-keyword-faces '(("DONE" . (:foreground "#006800" :strike-through t :height 160 :weight bold))
                            ("DELEGATED" . (:foreground "#feb24c" :slant italic :weight bold))
                            ("MABE" . (:foreground "#6e90c8" :slant italic :weight bold))
                            ("PROJ" . (:foreground "#b294bb" :slant italic :weight bold))
                            ("READ" . (:foreground "#9f7efe" :slant italic :weight bold))
                            ("WAITING" . (:foreground "coral" :slant italic :weight bold))))

  
  (org-log-done 'time)                      ; 记录时间
  (org-log-reschedule t)
  (org-log-redeadline t)
  (org-log-into-drawer t)                   ; hide org todo state changes in drawer or properties
  (org-drawers '("PROPERTIES" "LOGBOOK"))   ; separate drawers for clocking and logs
  (org-clock-into-drawer t)                 ; save clock data and state changes and notes in the LOGBOOK drawer
  (org-clock-out-remove-zero-time-clocks t) ; removes clocked tasks with 0:00 duration
  (org-clock-out-when-done t)               ; clock out when moving task to a done state

  (org-goto-interface 'outline-path-completion org-goto-max-level 10)

  ;; Blank lines.
  (org-blank-before-new-entry
   '(;; Insert  a blank line before new heading.
     (heading . t)

     ;; Try to make an intelligent decision whether to insert a
     ;; blank line or not before a new item.
     (plain-list-item . auto)))

  ;; Disabling underscore-to-subscript in Emacs Org-Mode export
  ;; @see - http://stackoverflow.com/questions/698562/disabling-underscore-to-subscript-in-emacs-org-mode-export/701201#701201
  (org-export-with-sub-superscripts nil) ; 全局禁用导出时 ^ 和 _ 作为上下标符号
  (org-pretty-entities-include-sub-superscripts nil)

  ;; 打开文件时，隐藏内容
  ;; Keep the headlines expanded in Org-Mode
  ;; @see - http://emacs.stackexchange.com/questions/9709/keep-the-headlines-expanded-in-org-mode
  (org-startup-folded "showall")

  ;; 所有标题按 4 个字符缩进
  (org-startup-indented t)             ; turn on header line 'org-indent-mode' by default
  (org-indent-indentation-per-level 3) ; indent, default value: '2'

  ;; 重新定义不同状态的 todoList 的排版
  ;; - http://sachachua.com/blog/2012/12/emacs-strike-through-headlines-for-done-tasks-in-org/
  (org-fontify-whole-heading-line t)
  (org-fontify-done-headline nil)
  (org-fontify-quote-and-verse-blocks t)

  (org-todo-keywords
   '((sequence "TODO(t!/!)" "NEXT(n!/!)" "DELEGATED(l!/!)" "|" "DONE(d!/!)" "DEFERRED(f!/!)" "CANCELLED(c@/!)")
     (sequence "MEETING" "|" "MEETING_DONE")
     (sequence "PROJ(p!/!)" "|" "PROJ_DONE(P!/!)")
     (sequence "READ(r!/!)" "|" "READ_DONE(R!/!)")
     (sequence "WAITING(w@/!)" "REPEAT(r!/!)" "MABE(m!/!)" "HOLD(h!/!)")))

  (org-use-fast-todo-selection t)
  (org-todo-state-tags-triggers
   '(("CANCELLED" ("CANCELLED" . t))
     ("WAITING" ("WAITING" . t))
     ("DELEGATED" ("delegated" . t))
     ("MABE" ("WAITING" . t))
     ("HOLD" ("WAITING") ("HOLD" . t))
     (done ("WAITING") ("HOLD"))
     ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
     ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
     ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))
  
  (org-pretty-entities t)
  (org-src-fontify-natively t)           ; Enable syntax highlighting in code blocks
  (org-indent-mode t)
  (org-use-property-inheritance t)
  (org-list-allow-alphabetical t)        ; have a. A. a) A) list bullets
  (org-src-preserve-indentation t)       ; 这个需要注释掉，会导致 org-src 里面的代码不会自动缩进两列
  ;; (org-edit-src-content-indentation 2)  ; 设置 org-src 里面的代码自动缩进 2 列
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)
  (org-hide-emphasis-markers t)          ; 隐藏 org-mode 语法中的标记字符
  (org-image-actual-width (/ (frame-pixel-width) 2))

  (org-id-link-to-org-use-id 'use-existing)
  ;; (org-image-actual-width '(400))
  :hook
  (org-mode . (lambda ()
                (text-scale-increase 1)
                (setq-local line-spacing 0.7)
                (setq-local truncate-lines t)
                (setq-local word-wrap nil)))
  :config
  (custom-set-faces
   '(org-done ((t (:strike-through t))))
   '(org-headline-done ((t (:strike-through t))))
   ;; 单独给 org-table 设一个等宽字体
   ;; 目前使用是有问题的，英文字符是按照设置的 Sarasa 字体，但是中文不是，导致还是不能正确对齐
   ;; Org table font
   ;; Sarasa Gothic SC
   ;; LXGW WenKai Mono GB
   '(org-drawer ((t (:family "Cascadia Code" :height 160))))
   '(org-table ((t (:family "LXGW Bright Code GB" :height 160)))))
  
  (defhydra hydra-org (:color red :columns 3)
    "Org Mode Movements"
    ("n" outline-next-visible-heading "next heading")
    ("p" outline-previous-visible-heading "prev heading")
    ("N" org-forward-heading-same-level "next heading at same level")
    ("P" org-backward-heading-same-level "prev heading at same level")
    ("u" outline-up-heading "up heading")
    ("C-l" yc/org-toggle-link-display "link")
    ("C-i" org-toggle-inline-images "image")
    ("g" org-goto "goto" :exit t))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (org . t)
     (shell . t)
     (sql . t)
     (go . t)
     (plantuml . t)))

  (defun add-pcomplete-to-capf ()
    (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
  (add-hook 'org-mode-hook #'add-pcomplete-to-capf)

  (defun yc/org-toggle-link-display ()
    "Toggle the literal or descriptive display of links."
    (interactive)
    (if org-descriptive-links
        (progn (org-remove-from-invisibility-spec '(org-link))
               (org-restart-font-lock)
               (setq org-descriptive-links nil))
      (progn (add-to-invisibility-spec '(org-link))
             (org-restart-font-lock)
             (setq org-descriptive-links t))))

  ;; Paste an image on clipboard to Emacs Org mode file.
  ;; - http://stackoverflow.com/questions/17435995/paste-an-image-on-clipboard-to-emacs-org-mode-file-without-saving-it
  ;; $ brew install pngpaste
  ;; 另外一个 screenshot 扩展 https://github.com/tecosaur/screenshot
  ;; - http://bianle.blog/2016/10/26/emacs-paste-image-from-clipboard/
  ;; - https://emacs-china.org/t/topic/6601/4
  ;; - https://stackoverflow.com/questions/17435995/paste-an-image-on-clipboard-to-emacs-org-mode-file-without-saving-it
  (defun yc/org-screenshot ()
    "Take a screenshot into a time stamped unique-named file in the
      same directory as the org-buffer and insert a link to this file."
    (interactive)
    (setq filename
          (concat
           (make-temp-name
            (concat (file-name-nondirectory (file-name-sans-extension buffer-file-name))
                    "_imgs/"
                    (format-time-string "%Y%m%d_%H%M%S_"))) ".png"))
    (unless (file-exists-p (file-name-directory filename))
      (make-directory (file-name-directory filename)))
    ;; take screenshot
    (if IS-MAC
        (call-process "screencapture" nil nil nil "-i" filename))
    (if IS-LINUX
        (call-process "import" nil nil nil filename))
    ;; insert into file if correctly taken
    (if (file-exists-p filename)
        (insert (concat "[[file:" filename "]]")))
    (org-display-inline-images))

  (defun yc/org-insert-clipboard-image-localdir ()
    (interactive)
    (setq filename
          (concat
           (make-temp-name
            (concat (file-name-nondirectory (file-name-sans-extension buffer-file-name))
                    "_imgs/"
                    (format-time-string "%Y%m%d_%H%M%S_"))) ".png"))
    (unless (file-exists-p (file-name-directory filename))
      (make-directory (file-name-directory filename)))

    (message (concat "/opt/homebrew/bin/pngpaste " (concat "\"" filename "\"")))
    (call-process-shell-command (concat "/opt/homebrew/bin/pngpaste " (concat "\"" filename "\"")))

    (insert (concat "[[file:" filename "]]")))

  (defun yc/copy-idlink-to-clipboard ()
    "Copy an ID link with the headline to killring, if no ID is there then create a new unique
ID.  This function works only in org-mode or org-agenda buffers.

The purpose of this function is to easily construct id:-links to
org-mode items. If its assigned to a key it saves you marking the
text and copying to the killring."
    (interactive)
    (when (eq major-mode 'org-agenda-mode) ;switch to orgmode
      (org-agenda-show)
      (org-agenda-goto))
    (when (eq major-mode 'org-mode) ; do this only in org-mode buffers
      (setq mytmphead (nth 4 (org-heading-components)))
      (setq mytmpid (funcall 'org-id-get-create))
      (setq mytmplink (format "[[id:%s][%s]]" mytmpid mytmphead))
      (kill-new mytmplink)
      (save-buffer)
      (message "Copied %s to killring (clipboard)" mytmplink)))

  (defun yc/counsel-rg-in-itsycnotes ()
    "rg in ~/notes"
    (interactive)
    (counsel-rg ivy-text "~/notes/" nil nil))
  
  (defun yc/org-insert-clipboard-image ()
    "paste image from clipboard"
    (interactive)
    (setq filename
          (concat
           (make-temp-name
            (concat "/Users/yangc/notes/images/"
                    (format-time-string "%Y%m%d_%H%M%S_"))) ".png"))
    ;;  (unless (file-exists-p (file-name-directory filename))
    ;;    (make-directory (file-name-directory filename)))
    (message (concat "/opt/homebrew/bin/pngpaste " (concat "\"" filename "\"")))
    (call-process-shell-command (concat "/opt/homebrew/bin/pngpaste " (concat "\"" filename "\"")))

    (insert (concat "[[file:" filename "]]")))

  ;; 不用零宽空格在 org-mode 中标记中文的办法，https://emacs-china.org/t/org-mode/22313
  ;; (font-lock-add-keywords 'org-mode
  ;;                         '(("\\cc\\( \\)[/+*_=~][^a-zA-Z0-9/+*_=~\n]+?[/+*_=~]\\( \\)?\\cc?"
  ;;                            (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) ""))))
  ;;                           ("\\cc?\\( \\)?[/+*_=~][^a-zA-Z0-9/+*_=~\n]+?[/+*_=~]\\( \\)\\cc"
  ;;                            (2 (prog1 () (compose-region (match-beginning 2) (match-end 2) "")))))
  ;;                         'append)
  )

;; Auto-show Markup Symbols/
;; This package makes it much easier to edit Org documents when org-hide-emphasis-markers is turned on.
;; It temporarily shows the emphasis markers around certain markup elements when you place your cursor
;; inside of them. No more fumbling around with = and * characters!
(use-package org-appear
  :hook (org-mode . org-appear-mode))

(use-package org-spacer
  :commands (org-spacer-enforce)
  :load-path "localelpa/org-spacer.el"
  :config
  (setq org-spacer-element-blanks
        '((0 headline)
          (1 paragraph src-block table property-drawer))))

;; moving images from point A to point B.
;; https://github.com/abo-abo/org-download
;; - https://github.com/jethrokuan/.emacs.d/blob/master/config.org#org-download
;; - https://coldnew.github.io/hexo-org-example/2018/05/22/use-org-download-to-drag-image-to-emacs/
(use-package org-download
  :disabled t
  :ensure-system-package (pngpaste . "brew install pngpaste")
  :after org
  :hook ((dired-mode . org-download-enable)
         (org-mode . org-download-enable))
  :config
  (org-download-enable)
  (if (memq window-system '(mac ns))
      (setq org-download-screenshot-method "screencapture -i %s")
    (setq org-download-screenshot-method "maim -s %s"))
  (defun yc/org-download-method (link)
    "This is a helper function for org-download.
    It creates a folder in the root directory (~/.org/img/) named after the
    org filename (sans extension) and puts all images from that file in there.
    Inspired by https://github.com/daviderestivo/emacs-config/blob/6086a7013020e19c0bc532770e9533b4fc549438/init.el#L701"
    (let ((filename
           (file-name-nondirectory
            (car (url-path-and-query
                  (url-generic-parse-url link)))))
          ;; Create folder name with current buffer name, and place in root dir
          (setq dirname "/Users/yangc/notes/images/")
          ;; (dirname (concat (file-name-nondirectory (file-name-sans-extension buffer-file-name)) "_imgs/"))
          ;; (dirname (concat "./images/"
          ;;                   (replace-regexp-in-string " " "_" (downcase (file-name-base buffer-file-name))))))
          )


      ;; Add timestamp to filename
      (setq filename-with-timestamp (format "%s%s.%s"
                                            (file-name-sans-extension filename)
                                            (format-time-string org-download-timestamp)
                                            (file-name-extension filename)))
      ;; Create folder if necessary
      (unless (file-exists-p dirname)
        (make-directory dirname))
      (expand-file-name filename-with-timestamp dirname)))
  (setq org-download-method 'yc/org-download-method))

;; lets you insert a link from your clipboard with a title that is fetched from the page’s metadata by curl.
;; https://github.com/rexim/org-cliplink
(use-package org-cliplink
  :commands (org-cliplink org-cliplink-clipboard-content)
  :bind
  (("C-c s-l" . org-store-link)
   ("C-c s-i" . org-cliplink)))

(use-package org-ol-tree
  :commands org-ol-tree
  :load-path "localelpa/org-ol-tree")

;; - https://github.com/takaxp/org-tree-slide
(use-package org-tree-slide
  :commands (org-tree-slide-mode org-tree-slide-skip-done-toggle)
  :custom
  (org-tree-slide-skip-outline-level 4)
  (org-tree-slide-skip-done nil)
  :config
  ;; (define-key org-mode-map (kbd "<f8>") 'org-tree-slide-mode)
  ;; (define-key org-mode-map (kbd "S-<f8>") 'org-tree-slide-skip-done-toggle)
  ;; (define-key org-tree-slide-mode-map (kbd "<f9>") 'org-tree-slide-move-previous-tree)
  ;; (define-key org-tree-slide-mode-map (kbd "<f10>") 'org-tree-slide-move-next-tree)
  ;; (define-key org-tree-slide-mode-map (kbd "<f11>") 'org-tree-slide-content)
  (org-tree-slide-narrowing-control-profile))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :custom
  ;; Make leading stars truly invisible, by rendering them as spaces!
  (org-superstar-leading-bullet ?\s)
  (org-superstar-leading-fallback ?\s)
  (org-superstar-special-todo-items nil)
  (org-superstar-headline-bullets-list
   '("⚫" "⚫" "⚫" "⚫" "⚫" "⚫" "⚫" "⚫"))
  (org-superstar-item-bullet-alist
   '((?* . ?☯)
     (?+ . ?✚)
     (?- . ?▶))))

;; - https://github.com/jakebox/preview-org-html-mode
(use-package preview-org-html-mode
  :load-path "localelpa/preview-org-html-mode"
  :custom
  (preview-org-html-viewer 'xwidget)
  :bind
  (:map org-mode-map
        ([f12] . preview-org-html-mode)))

(use-package ob-go)

(use-package ox-gfm)
(eval-after-load "org"
  '(require 'ox-gfm nil t))

;; https://github.com/kawabata/ox-pandoc
(use-package ox-pandoc
  :config
  (with-eval-after-load 'ox
    (require 'ox-pandoc))
  ;; special settings for beamer-pdf and latex-pdf exporters
  (setq org-pandoc-options-for-beamer-pdf '((pdf-engine . "xelatex")))
  (setq org-pandoc-options-for-latex-pdf '((pdf-engine . "pdflatex"))))

(use-package ox-beamer
  :ensure nil
  :config
  (with-eval-after-load 'ox
    (defun eli-strip-ws-maybe (text _backend _info)
      (let* ((text (replace-regexp-in-string
                    "\\(\\cc\\) *\n *\\(\\cc\\)"
                    "\\1\\2" text));; remove whitespace from line break
             ;; remove whitespace from `org-emphasis-alist'
             (text (replace-regexp-in-string "\\(\\cc?\\) \\(.*?\\) \\(\\cc\\)"
                                             "\\1\\2\\3" text))
             ;; restore whitespace between English words and Chinese words
             (text (replace-regexp-in-string "\\(\\cc\\)\\(\\(?:<[^>]+>\\)?[a-z0-9A-Z-]+\\(?:<[^>]+>\\)?\\)\\(\\cc\\)"
                                             "\\1 \\2 \\3" text))
             (text (replace-regexp-in-string "\\(\\cc\\) ?\\(\\\\[^{}()]*?\\)\\(\\cc\\)"
                                             "\\1 \\2 \\3" text)))
        text))
    (add-to-list 'org-export-filter-paragraph-functions #'eli-strip-ws-maybe))
  (progn
    ;; Allow for export=>beamer by placing
    ;; #+latex_class: beamer in Org files
    (add-to-list 'org-latex-classes
                 '("beamer"
                   "\\documentclass[presentation]{beamer}"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))))

(use-package htmlize)

;;================================================================================
;; 当执行 org code block 后，显示图片
;; - http://orgmode.org/worg/org-hacks.html#orgheadline126
;; (defun yc/ogrep (search &optional context)
;;   "Search for word in org files.
;;     Prefix argument determines number of lines."
;;   (interactive "sSearch for: \nP")
;;   (let ((grep-find-ignored-files '("#*" ".#*"))
;;         (grep-template (concat "grep <X> -i -nH "
;;                                (when context
;;                                  (concat "-C" (number-to-string context)))
;;                                " -e <R> <F>")))
;;     (lgrep search "*org*" org-directory)))

;; (add-hook 'org-babel-after-execute-hook 'yc/display-inline-images 'append)

;; (defun yc/display-inline-images ()
;;   (condition-case nil
;;       (org-display-inline-images)
;;     (error nil)))

;; - https://github.com/howardabrams/dot-files/blob/master/emacs-client.org
;; - https://zzamboni.org/post/beautifying-org-mode-in-emacs/
;; 将列标记符号替换成 ► ◇ 表示
;; @see - http://www.howardism.org/Technical/Emacs/orgmode-wordprocessor.html
;; (font-lock-add-keywords 'org-mode
;;                         '(("^ *\\([-+]\\) "
;;                            (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "━"))))))

;; (font-lock-add-keywords 'org-journal-mode
;;                         '(("^ *\\([-+]\\) "
;;                            (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "━"))))))

;; todo keywords 的背景色设置可以参考这个： /Users/yangc/src/emacs.d/emacs-leuven/docs/index.org::*Extended use of TODO keywords
;; 参考2 - file:~/src/emacs.d/emagicians-starter-kit/Org-Grimoire.org::*Faces
;; (defun custom-org-faces ()
;;   "Custom org face"
;;   (interactive)
;;   (let* (;; (variable-tuple (cond ;; ((x-list-fonts "Monaco")          '(:font "Monaco"))
;;          ;; ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
;;          ;; ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
;;          ;; ((x-list-fonts "Verdana")         '(:font "Verdana"))
;;          ;; ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
;;          ;; (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
;;          (base-font-color     (face-foreground 'default nil 'default))
;;          (headline           `(:inherit default :height 1.0))
;;          (headline-1         `(:inherit default :weight bold)))

;;     (custom-theme-set-faces 'user
;;                             `(org-level-8 ((t (,@headline))))
;;                             `(org-level-7 ((t (,@headline))))
;;                             `(org-level-6 ((t (,@headline))))
;;                             `(org-level-5 ((t (,@headline))))
;;                             `(org-level-4 ((t (,@headline))))
;;                             `(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))))
;;                             ;; `(org-headline-done ((t (:foreground "#4f5b66" :strike-through t))))  ;; (:foreground "LightSalmon" :strike-through t)
;;                             `(org-headline-done ((t (:foreground "#999" :strike-through t))))
;;                             `(org-level-3 ((t (,@headline))))
;;                             `(org-level-2 ((t (,@headline :height 1.0))))
;;                             `(org-level-1 ((t (,@headline-1 :height 1.2))))
;;                             `(org-document-title ((t (,@headline-1  :height 1.5 :underline nil))))
;;                             `(org-link ((t (:underline t))))))
;;   )

;; (add-hook 'org-mode-hook #'custom-org-faces)

;; (custom-set-faces
;;  '(org-block-begin-line
;;    ((t (:underline "#A7A6AA" :foreground "#333333" :background "#444444" :height 0.9 :slant italic :weight semi-bold))))
;;  '(org-block-end-line
;;    ((t (:overline "#A7A6AA" :foreground "#333333" :background "#444444" :height 0.9 :slant italic :weight semi-bold))))
;;  '(org-block
;;    ((t (:background "#333333"))))
;;  '(org-block-background
;;    ((t (:background "#333333"))))
;;  )

;; (custom-set-faces
;;  '(org-block-begin-line
;;    ((t (:underline "#A7A6AA" :foreground "#666666" :background "#EBDAB4" :height 0.9 :slant italic :weight semi-bold))))
;;  '(org-block-end-line
;;    ((t (:overline "#A7A6AA" :foreground "#666666" :background "#EBDAB4" :height 0.9 :slant italic :weight semi-bold))))
;;  '(org-block
;;    ((t (:background "#F2E4BE"))))
;;  '(org-block-background
;;    ((t (:background "#F2E4BE"))))
;;  )

;; (custom-set-faces
;;  '(org-block
;;    ((t (:foreground "#184034" :background "#F0F0F0"))))
;;  '(org-block-background
;;    ((t (:foreground "#184034" :background "#F0F0F0"))))
;;  )

;; (custom-set-faces
;;  '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))))
;;  '(org-headline-done
;;    ((((class color) (min-colors 16) (background dark))
;;      (:foreground "#4f5b66" :strike-through t)))))

;; (:foreground "LightSalmon" :strike-through t)

;; Need org-fontify-done-heading=t
;; (defun modify-org-done-face ()
;;   (set-face-attribute 'org-done nil :strike-through t)
;;   (set-face-attribute 'org-headline-done nil :strike-through t))

;; (eval-after-load "org" (add-hook 'org-add-hook 'modify-org-done-face))

;; (defun org-insert-src-block (src-code-type)
;;   "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
;;   (interactive
;;    (let ((src-code-types
;;           '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
;;             "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
;;             "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
;;             "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
;;             "scheme" "sqlite")))
;;      (list (ido-completing-read "Source code type: " src-code-types))))
;;   (progn
;;     (newline-and-indent)
;;     (insert (format "#+begin_src %s\n" src-code-type))
;;     (newline-and-indent)
;;     (insert "#+end_src\n")
;;     (previous-line 2)
;;     (org-edit-src-code)))

;; 定义一部分在 orgmode 下编写代码块的快捷模板，此快捷模板可以通过 =(<s[TAB])= 的方式快捷输入模板块，如下以此类推，输入 =(<e[TAB])= 即可输入另外的模板。
;; Use org-tempo to allow inserting templates using
;; e.g. =<s=

;; 可以参考 /Users/yangc/src/emacs.d/sachac-emacs.d/Sacha.org::*Structure templates
;; 模板元素说明： @see - https://www.cnblogs.com/holbrook/archive/2012/04/17/2454619.html
;; https://www.gnu.org/software/emacs/manual/html_node/org/Template-elements.html#Template-elements
;; https://www.gnu.org/software/emacs/manual/html_node/org/Template-expansion.html#Template-expansion
;; https://www.reddit.com/r/emacs/comments/ad68zk/get_easytemplates_back_in_orgmode_92/?utm_source=dlvr.it&utm_medium=twitter
;; https://gist.github.com/takaxp/4fb109c2dcc67b8d8de4914760977674

;; (require 'org-tempo)
;; (when (version< "9.2" (org-version))
;;       (add-to-list 'org-modules 'org-tempo))
;; (add-to-list 'org-structure-template-alist
;;                (if (version< "9.2" (org-version))
;;                    '(?S . "src emacs-lisp")
;;                  '("S" "#+begin_src emacs-lisp\n?\n#+end_src" "<src lang=\"emacs-lisp\">\n\n</src>")))

;; (setq org-structure-template-alist
;;       '(("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")
;;         ("e" "#+begin_example\n?\n#+end_example" "<example>\n?\n</example>")
;;         ("q" "#+begin_quote\n?\n#+end_quote" "<quote>\n?\n</quote>")
;;         ("v" "#+begin_verse\n?\n#+end_verse" "<verse>\n?\n</verse>")
;;         ("c" "#+begin_comment\n?\n#+end_comment")
;;         ("p" "#+begin_practice\n?\n#+end_practice")
;;         ("o" "#+begin_src emacs-lisp :tangle yes\n?\n#+end_src" "<src lang=\"emacs-lisp\">\n?\n</src>")
;;         ("l" "#+begin_src emacs-lisp\n?\n#+end_src" "<src lang=\"emacs-lisp\">\n?\n</src>")
;;         ("L" "#+latex: " "<literal style=\"latex\">?</literal>")
;;         ("h" "#+begin_html\n?\n#+end_html" "<literal style=\"html\">\n?\n</literal>")
;;         ("H" "#+html: " "<literal style=\"html\">?</literal>")
;;         ("a" "#+begin_ascii\n?\n#+end_ascii")
;;         ("A" "#+ascii: ")
;;         ("i" "#+index: ?" "#+index: ?")
;;         ("I" "#+include %file ?" "<include file=%file markup=\"?\">")))

;; visual indent
;; (use-package org-bars
;;   :straight (:host github :repo "tonyaldon/org-bars")
;;   :hook (org-mode . org-bars-mode))

;; - https://github.com/stardiviner/org-link-beautify

;; or https://github.com/stardiviner/org-tag-beautify
;; (use-package org-pretty-tags
;;   :demand t
;;   :config
;;   (setq org-pretty-tags-surrogate-strings
;;         (quote
;;          (("TOPIC" . "☆")
;;           ("PROJEKT" . "💡")
;;           ("SERVICE" . "✍")
;;           ("Blog" . "✍")
;;           ("music" . "♬")
;;           ("security" . "🔥"))))
;;   (org-pretty-tags-global-mode))

(use-package org-fancy-priorities
  :disabled t
  :defines org-fancy-priorities-list
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (unless (char-displayable-p ?❗)
    (setq org-fancy-priorities-list '("HIGH" "MID" "LOW" "OPTIONAL"))))

;; (use-package org-num
;;   :after org)

;; - https://github.com/casouri/valign
;; - https://github.com/casouri/ftable
;; - https://github.com/Fuco1/org-pretty-table
(use-package valign
  :disabled t
  :after org
  :hook ((org-mode . valign-mode)
         (markdown-mode . valign-mode))
  :custom
  (valign-max-table-size 10000)  ;; 0 to valign doesn’t check for table sizes.
  (valign-fancy-bar t))

(use-package org-pretty-table
  :disabled t
  :load-path "localelpa/org-pretty-table"
  :hook
  (org-mode . org-pretty-table-mode))

;; (use-package ftable
;;   :straight (:host github
;;                 :repo "casouri/ftable"
;;                 :files ("*.el"))
;; )

;; Emacs Doom E15: Fancy Task Priorities in Org Mode
;; 代码来源：https://www.reddit.com/r/emacs/comments/ctfxbg/emacs_doom_e15_fancy_task_priorities_in_org_mode/
;; pretty-symbols 参考配置 file:~/src/emacs.d/yantar92-emacs-config
;; 看了下 pretty-symbols 的文档，这个作者已经不维护了，建议是采用 prettify-symbols 替换
(use-package pretty-symbols
  :disabled t
  :hook
  (org-mode . pretty-symbols-mode)
  :config
  (setq pretty-symbol-categories '(relational logical lambda org-specific nil cpp general))

  (defun yant/str-to-glyph (str)
    "Transform string into glyph, displayed correctly."
    (let ((composition nil))
      (dolist (char (string-to-list str)
                    (nreverse (cdr composition)))
        (push char composition)
        (push '(Br . Bl) composition))))

  (setq pretty-symbol-patterns
        (append pretty-symbol-patterns
                '((?▤ org-specific ":LOGBOOK:" (org-mode))
                  (?⚙ org-specific ":PROPERTIES:" (org-mode))
                  (?⏏ org-specific ":END:" (org-mode))
                  (?★ org-specific "\\[#A\\]" (org-mode))
                  ("" org-specific "\\[#B\\]" (org-mode))
                  (?☕ org-specific "\\[#C\\]" (org-mode))
                  (?⁂ org-specific "\\(^\\*\\)[^*]" (org-mode) 1)
                  (?• org-specific "^\\(?:\\*\\{1\\}\\)\\(\\*\\)[^*]" (org-mode) 1)
                  
                  (?⋮ org-specific "^\\(?:\\*\\{3\\}\\)\\(\\*\\)[^*]" (org-mode) 1)
                  (?⋱ org-specific "^\\(?:\\*\\{4,\\}\\)\\(\\*\\)[^*]" (org-mode) 1)
                  ((yant/str-to-glyph "☐") org-specific "\\(?:^*+ +\\)\\(\\<TODO\\>\\)" (org-mode) 1)
                  ((yant/str-to-glyph "☑") org-specific "\\(?:^*+ +\\)\\(\\<DONE\\>\\)" (org-mode) 1)
                  ((yant/str-to-glyph "⇛") org-specific "\\(?:^*+ +\\)\\(\\<DELEGATED\\>\\)" (org-mode) 1)
                  ((yant/str-to-glyph "⌁") org-specific "\\(?:^*+ +\\)\\(\\<READ\\>\\)" (org-mode) 1)
                  ((yant/str-to-glyph "⌗") org-specific "\\(?:^*+ +\\)\\(\\<MABE\\>\\)" (org-mode) 1)
                  ((yant/str-to-glyph "✘") org-specific "\\(?:^*+ +\\)\\(\\<FAILED\\>\\)" (org-mode) 1)
                  ((yant/str-to-glyph "✘") org-specific "\\(?:^*+ +\\)\\(\\<CANCELLED\\>\\)" (org-mode) 1)
                  ((yant/str-to-glyph "▶") org-specific "\\(?:^*+ +\\)\\(\\<NEXT\\>\\)" (org-mode) 1)
                  ((yant/str-to-glyph "☇") org-specific "\\(?:^*+ +\\)\\(\\<MERGED\\>\\)" (org-mode) 1)
                  ((yant/str-to-glyph "✍") org-specific "\\(?:^*+ +\\)\\(\\<INBO\\>\\)" (org-mode) 1)
                  ((yant/str-to-glyph "⚑") org-specific "\\(?:^*+ +\\)\\(\\<WAITING\\>\\)" (org-mode) 1)
                  ((yant/str-to-glyph "♲") org-specific "\\(?:^*+ +\\)\\(\\<HOLD\\>\\)" (org-mode) 1)
                  ((yant/str-to-glyph "☠D") org-specific "\\<DEADLINE:" (org-mode))
                  ((yant/str-to-glyph "◴S") org-specific "\\<SCHEDULED:" (org-mode)))))
  )

(use-package prettify-symbols
  :disabled t
  :config
  ;; (add-hook 'org-mode-hook (lambda ()
  ;;                            "Beautify Org Checkbox Symbol"
  ;;                            (push '("[ ]" . "☐") prettify-symbols-alist)
  ;;                            (push '("[X]" . "☑" ) prettify-symbols-alist)
  ;;                            (push '("[-]" . "❍" ) prettify-symbols-alist)
  ;;                            (prettify-symbols-mode)))

  (setq-default prettify-symbols-alist '(
                                         ;; ("#+begin_src" . "†")
                                         ;; ("#+end_src" . "†")
                                         ("#+begin_src" . "ƛ")
                                         ("#+end_src" . "ƛ")
                                         ("#+begin_quote" . "†")
                                         ("#+end_quote" . "†")
                                         (">=" . "≥")
                                         ("=>" . "⇨")))
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  (add-hook 'org-mode-hook 'prettify-symbols-mode)
  ;; (global-prettify-symbols-mode t)
  )

(use-package org-treescope
  :disabled t
  :commands (org-treescope)
  :custom
  (org-treescope-query-userbuffer "~/path/to/projects.txt")
  (org-treescope-cyclestates-todo '(nil ("TODO") ("WAITING" "DONE")))
  (org-treescope-cyclestates-priority '(nil ("A" "B" "C") ("D"))))

;; 解决在中文环境下，样式字符需要前后添加空格才能正确格式化的问题
;; 解决思路是自动插入一个不可见的空格字符
;; 详细讨论参考文章：https://emacs-china.org/t/org-mode/597/61
(use-package separate-inline
  :disabled t
  :load-path "localelpa/separate-inline.el"
  :hook ((org-mode-hook . separate-inline-mode)
         (org-mode-hook . (lambda ()
                            (add-hook 'separate-inline-mode-hook
                                      'separate-inline-use-default-rules-for-org-local
                                      nil 'make-it-local)))))

;; vi-like bindings for org-mode
;; - https://oremacs.com/worf/README.html
;; (use-package worf
;;   :hook
;;   (org-mode . worf-mode))

;; Insert org-mode links to items selected in various Mac apps.
;; 参考这篇内容：http://orgmode.org/worg/org-contrib/org-mac-link.html
;; 完成配置 org-mac-link
;; (use-package org
;;   ;; :straight org-plus-contrib
;;   :init
;;   (require 'org-mac-link)
;;   ;; (add-hook 'org-mode-hook (lambda ()
;;   ;;                 (define-key org-mode-map (kbd "C-c s-g") 'org-mac-grab-link)))
;;   :bind
;;   (:map org-mode-map ("C-c s-g" . org-mac-grab-link)))

;; TODO electric-quote-mode

(use-package org-sticky-header
  :disabled t
  :hook (org-mode . org-sticky-header-mode)  
  :config
  ;; Show full path in header
  (setq org-sticky-header-full-path 'full)
  ;; Use > instead of / as separator
  (setq org-sticky-header-outline-path-separator " > "))

;; - https://github.com/alphapapa/org-sidebar
(use-package org-sidebar
  :disabled t
  :commands (org-sidebar org-sidebar-tree)
  ;; :straight (:host github :repo "alphapapa/org-sidebar")
  )

;; Table-of-contents sidebar for Emacs.
;; 暂时还没在 mpla 里
(use-package outline-toc
  :disabled t)

;; (setq org-publish-project-alist
;;       '(
;;         ("org-blog-content" ;; 博客内容
;;          ;; Path to your org files.
;;          :base-directory "/Users/yangc/Dropbox/itsycnotes/"
;;          :base-extension "org"
;;          ;; Path to your jekyll project.
;;          :publishing-directory "~/Applications/nginx/notes/"
;;          :recursive t
;;          :publishing-function org-html-publish-to-html
;;          :headline-levels 4
;;          :html-extension "html"
;;          :table-of-contents t ;; 导出目录
;;          :link-home "home.html"
;;          :html-preamble (concat "INSERT HTML CODE HERE FOR PREAMBLE")
;;          :html-postamble (concat "INSERT HTML CODE HERE FOR POSTAMBLE")
;;          ;; :body-only t ;; Only export section between <body></body>
;;          )
;;         ("org-blog-static" ;; 静态文件
;;          :base-directory "/Users/yangc/Dropbox/itsycnotes/"
;;          :base-extension "css\\|ico\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php\\|svg"
;;          :publishing-directory "~/Applications/nginx/notes/"
;;          :recursive t
;;          :publishing-function org-publish-attachment)
;;         ("blog" :components ("org-blog-content" "org-blog-static"))
;;         ))

;; auto load markdown exporter when load org-mode
;; (eval-after-load "org"
;;   '(require 'ox-md nil t))

;; MobileOrg
;; documentation: https://mobileorg.github.io/documentation/
;; 执行命令进行推送： =M-x org-mobile-push= or =C-c C-x RET p= 。
;; ;; Set to the name of the file where new notes will be stored
;; (setq org-mobile-inbox-for-pull (concat org-directory "inbox.org"))
;; ;; Set to <your Dropbox root directory>/MobileOrg.
;; (setq org-mobile-directory "~/Dropbox/应用/MobileOrg")

;; Exports Org-mode contents to Reveal.js HTML presentation.
(use-package ox-reveal
  :disabled t
  ;; :straight (:host github :repo "yjwen/org-reveal")
  :after org
  :init
  (setq org-reveal-mathjax t)
  :config
  (require 'ox-reveal)
  ;; (setq org-reveal-root "https://cdnjs.cloudflare.com/ajax/libs/reveal.js/3.7.0"))
  (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
  (setq org-reveal-theme "black")
  (setq org-reveal-width 1200)
  (setq org-reveal-height 1000)
  (setq org-reveal-margin "0.1")
  (setq org-reveal-min-scale "0.5")
  (setq org-reveal-max-scale "2.5")
  (setq org-reveal-transition "cube")
  (setq org-reveal-plugins '(classList markdown zoom notes))
  (setq org-reveal-control t)
  (setq org-reveal-center t)
  (setq org-reveal-progress t)
  (setq org-reveal-history nil))

;; Evaluate org-src-block asynchronously.
;; https://github.com/astahlman/ob-async
;; 如果要加这个，是不是要在 begin_src 上加上关键字，还是默认全部都执行，对 emacs dotfile 的加载有没有影响，这些都要验证。
(use-package ob-async
  :disabled t
  :after org
  :commands (ob-async-org-babel-execute-src-block)
  ;; :init
  ;; (with-eval-after-load 'org
  ;;   (add-to-list 'org-ctrl-c-ctrl-c-hook 'ob-async-org-babel-execute-src-block))
  )

;; Visually summarize progress
;; (use-package org-dashboard)

;; - https://github.com/rksm/clj-org-analyzer
(use-package org-analyzer
  :disabled t
  :commands (org-analyzer-start))

;; (defhydra hydra-org (:color red)
;;   "
;;   ^
;;   ^Quit^           ^Move^                 ^Zoom^
;;   ^───────^───────────^──────^────────────^────^──────────
;;   _q_ quit            _n_ Next Head        _<_ Go Back
;;   ^^                  _p_ Prev Head        _l_ →
;;   ^^                  _N_ Next Same Head   _h_ ←
;;   ^^                  _P_ Prev Same Head   _k_ ↑
;;   ^^                  _u_ Up               _j_ ↓
;;   ^^                  _g_ Goto             ^^
;;   ^^                  ^^                   ^^
;;   ^^                  ^^                   ^^
;;   "
;;   ("n" outline-next-visible-heading "next heading")
;;   ("p" outline-previous-visible-heading "prev heading")
;;   ("N" org-forward-heading-same-level "next heading at same level")
;;   ("P" org-backward-heading-same-level "prev heading at same level")
;;   ("u" outline-up-heading "up heading")
;;   ("g" org-goto "goto" :exit t)
;;   ("<" worf-back-to-heading "worf-back-to-heading")
;;   ("l" worf-right "worf-right")
;;   ("j" worf-down "worf-down")
;;   ("k" worf-up "worf-up")
;;   ("h" worf-left "worf-left")
;;   ("q" nil))

;; UTF-8 bullets for org-mode.
;; - https://github.com/sabof/org-bullets
;; - https://github.com/integral-dw/org-superstar-mode
;; (use-package org-bullets
;;   :if (char-displayable-p ?⚫)
;;   :hook (org-mode . org-bullets-mode)
;;   ;; ◎ ⊛ ✪ ☯ ⊙ ➲ ● 🌑 ⬤ ⚉ ⸖ ͼ ͽ ○ ￮ ⚬ ⦾ ◦ ∙ ∘ ⚪ ◯ ⌾ ⎉ ⎊ ☉ ✿ ☀ ⚆ ⚇ ⚈ ⚝ ☼ ⚭ ⛒  ⛮ ⚫ ⌀  ⍟ ⎔
;;   ;; (setq org-bullets-bullet-list '("☀" "✪" "❂" "✸" "☼" "☉" "⊛" "◉" "◌"))
;;   :init
;;   (setq org-bullets-bullet-list '("⚫" "⚫" "⚫" "⚫" "⚫" "⚫" "⚫" "⚫")))

;; https://github.com/minad/org-modern
(use-package org-modern
  :disabled t
  ;; :straight (:host github :repo "minad/org-modern")
  :hook (org-mode . org-modern-mode))

(use-package org-modern-indent
  :load-path "localelpa/org-modern-indent"
  :disabled t
  :hook
  (org-mode . org-modern-indent-mode))
;;================================================================================

(provide 'init-org)
;;; init-org.el ends here
