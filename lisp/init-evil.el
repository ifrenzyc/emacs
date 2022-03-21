;; init-evil.el --- Initialize evil settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; - https://github.com/emacs-evil/evil
;; - http://wikemacs.org/wiki/Evil
;; - https://github.com/noctuid/evil-guide#terminology
;; 

;;; Code

(require 'init-undo-redo)

(use-package evil-collection
  :init
  (setq evil-want-keybinding nil
        evil-collection-company-use-tng nil
        evil-want-integration nil)
  :config
  (evil-collection-init 'compile)
  (evil-collection-init 'info)
  (evil-collection-init 'custom)
  (evil-collection-init 'dired)
  (evil-collection-init 'minibuffer)
  (evil-collection-init 'flycheck)
  (evil-collection-init 'xref)
  (evil-collection-init 'magit)
  (evil-collection-init 'which-key))

(use-package evil
  ;; :demand t
  :init
  (setq evil-want-keybinding nil
        evil-want-integration nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump t
        evil-visual-state-cursor '(hbar . 2)
        evil-insert-state-cursor '(bar . 2)
        evil-respect-visual-line-mode t
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-undo-system 'undo-tree) ;; undo-fu
  (setq-default evil-want-Y-yank-to-eol t)
  :hook
  ((after-init . evil-mode)
   (evil-local-mode . turn-on-undo-tree-mode))
  ;; ;; 指定哪些 mode 才启用 evil-mode https://github.com/louisch/dotemacs/blob/a354bbf1f9b9854a32353dd65b34557f0e594a1a/personal/package-config.el
  ;; (prog-mode . evil-local-mode)
  ;; (fundamental-mode . evil-local-mode)
  ;; (text-mode . evil-local-mode)
  :general
  ((evil-normal-state-map [escape] 'keyboard-quit)
   (evil-visual-state-map [escape] 'keyboard-quit)
   (minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
   (minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
   (minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
   (minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
   (minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit))

  ;; (evil-normal-state-map "\C-y" 'yank)
  ;; (evil-insert-state-map "\C-y" 'yank)
  ;; (evil-visual-state-map "\C-y" 'yank)
  ;; (evil-insert-state-map "\C-e" 'end-of-line)
  :config
  ;; (OPTIONAL) Shift width for evil-mode users
  ;; For the vim-like motions of ">>" and "<<".
  ;; (setq-default evil-shift-width 4)

  ;; (setq evil-emacs-state-cursor '("orange" box)
  ;;       evil-normal-state-cursor '("white" box)
  ;;       evil-visual-state-cursor '("white" (hbar . 2))
  ;;       evil-insert-state-cursor '("white" (bar . 2))
  ;;       evil-replace-state-cursor '("white" (hbar . 2))
  ;;       evil-operator-state-cursor '("white" hollow))

  ;; @see https://bitbucket.org/lyro/evil/issue/360/possible-evil-search-symbol-forward
  ;; evil 1.0.8 search word instead of symbol
  (setq evil-symbol-word-search t)

  ;; @see - https://github.com/rime/squirrel/wiki/vim 用户与 emacs-evil-mode 用户-输入法自动切换成英文状态的实现
  (defadvice keyboard-quit (before evil-insert-to-nornal-state activate)
    "C-g back to normal state"
    (when (evil-insert-state-p)
      (cond
        ((equal (evil-initial-state major-mode) 'normal)
         (evil-normal-state))
        ((equal (evil-initial-state major-mode) 'insert)
         (evil-normal-state))
        ((equal (evil-initial-state major-mode) 'motion)
         (evil-motion-state))
        (t
         (if (equal last-command 'keyboard-quit)
             (evil-normal-state)         ; 如果初始化 state 不是 normal，按两次才允许转到 normal state
           (evil-change-to-initial-state)) ; 如果初始化 state 不是 normal，按一次转到初始状态
         ))))

  ;; esc quits
  ;; @see - http://stackoverflow.com/questions/8483182/evil-mode-best-practice
  (defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
    In Delete Selection mode, if the mark is active, just deactivate it;
    then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))

  (dolist (p '((dired-mode . emacs)
               (rg-mode . emacs)
               (undo-tree-visualizer-mode . emacs)
               (ivy-occur-mode . emacs)
               (help-mode . emacs)
               (helpful-mode . emacs)
               (profiler-report-mode . emacs)
               (diff-mode . emacs)
               (color-rg-mode . emacs)
               (xref--xref-buffer-mode . emacs)
               (treemacs-mode . emacs)
               (flycheck-errors-mode . emacs)
               (vterm-mode . emacs)
               (eshell-mode . emacs)
               (term-mode . emacs)
               (lsp-ui-imenu-mode . emacs)
               (bufler-list-mode . emacs)
               (org-agenda-mode . emacs)
               (org-brain-visualize-mode . emacs)))
    (evil-set-initial-state (car p) (cdr p))))

;; let there be a <leader>
;; https://github.com/cofi/evil-leader
(use-package evil-leader
  :init
  (global-evil-leader-mode 1)
  (evil-leader/set-leader ",")
  :config
  (evil-leader/set-key
      "SPC" 'hydra-rotate-window/body
    "1" 'delete-other-windows
    "0" 'delete-window
    "5" 'delete-frame
    "f" 'project-find-file
    "F" 'find-file
    "t" 'multi-vterm-project
    "r" 'counsel-recentf
    "b" 'switch-to-buffer))

;; - https://github.com/unblevable/quick-scope.
;; [[file:./screenshots/evil-quickscope.gif]]
;; This package emulates. It highlights targets for evil-mode’s f,F,t,T keys, allowing for quick navigation within a line with no additional mappings.
;; evil-mode 默认的 ~f/F~, ~t/T~ 是绑定 ~evil-find-char~, ~evil-find-char-to~ 方法的，这里将这两个按键
;; 绑定到 ~evil-quickscope-find-char~ 和 ~evil-quickscope-find-char-to~ 方法上。
;; 然后增加 ~s/S~ 按键绑定 ~evil-snipe-s/S~ 上

(use-package evil-quickscope
  :demand t
  :after evil
  :config
  (global-evil-quickscope-mode))

;; - https://github.com/hlissner/evil-snipe
;; 2-char searching with ~f~, ~F~, ~t~, ~T~, ~s~, ~S~ operators. Like seek.vim/sneak.vim

(use-package evil-snipe
  :demand t
  :after (evil-quickscope)
  :init
  (setq evil-snipe-scope 'line ;; 'whole-buffer 'visible
        evil-snipe-repeat-scope 'line
        evil-snipe-enable-highlight t
        evil-snipe-enable-incremental-highlight t
        evil-snipe-auto-disable-substitute t
        evil-snipe-show-prompt nil
        evil-snipe-smart-case t)
  :config
  (evil-snipe-mode t)
  ;; (evil-snipe-override-mode t)
  :general
  ;; single letter keybindings for operator/visual
  (general-def '(normal motion operator visual)
    "s" #'evil-snipe-s
    "S" #'evil-snipe-S)
  ;; :general
  ;; (evil-leader/set-key
  ;;   "s" 'evil-snipe-s
  ;;   "S" 'evil-snipe-S)
  ;; :config
  ;; (define-key evil-snipe-parent-transient-map (kbd "C-;"))
  )

(use-package evil-easymotion
  :demand t
  :after evil
  :config
  (evilem-default-keybindings "gs")
  ;; Use evil-search backend, instead of isearch
  (evilem-make-motion evilem-motion-search-next #'evil-ex-search-next
                      :bind ((evil-ex-search-highlight-all nil)))
  (evilem-make-motion evilem-motion-search-previous #'evil-ex-search-previous
                      :bind ((evil-ex-search-highlight-all nil)))
  (evilem-make-motion evilem-motion-search-word-forward #'evil-ex-search-word-forward
                      :bind ((evil-ex-search-highlight-all nil)))
  (evilem-make-motion evilem-motion-search-word-backward #'evil-ex-search-word-backward
                      :bind ((evil-ex-search-highlight-all nil)))

  ;; Rebind scope of w/W/e/E/ge/gE evil-easymotion motions to the visible
  ;; buffer, rather than just the current line.
  (put 'visible 'bounds-of-thing-at-point (lambda () (cons (window-start) (window-end))))
  (evilem-make-motion evilem-motion-forward-word-begin #'evil-forward-word-begin :scope 'visible)
  (evilem-make-motion evilem-motion-forward-WORD-begin #'evil-forward-WORD-begin :scope 'visible)
  (evilem-make-motion evilem-motion-forward-word-end #'evil-forward-word-end :scope 'visible)
  (evilem-make-motion evilem-motion-forward-WORD-end #'evil-forward-WORD-end :scope 'visible)
  (evilem-make-motion evilem-motion-backward-word-begin #'evil-backward-word-begin :scope 'visible)
  (evilem-make-motion evilem-motion-backward-WORD-begin #'evil-backward-WORD-begin :scope 'visible)
  (evilem-make-motion evilem-motion-backward-word-end #'evil-backward-word-end :scope 'visible)
  (evilem-make-motion evilem-motion-backward-WORD-end #'evil-backward-WORD-end :scope 'visible))

;; - https://github.com/TheBB/evil-indent-plus
;; evil-indent-plus defines text objects for block of code that has same/higher indentation.
(use-package evil-indent-plus
  :after evil
  ;; :config
  ;; bind evil-indent-plus text objects
  ;; (evil-indent-plus-default-bindings)
  :general
  (evil-inner-text-objects-map
   "i" 'evil-indent-plus-i-indent
   "I" 'evil-indent-plus-i-indent-up
   "j" 'evil-indent-plus-i-indent-up-down)
  (evil-outer-text-objects-map
   "i" 'evil-indent-plus-a-indent
   "I" 'evil-indent-plus-a-indent-up
   "J" 'evil-indent-plus-a-indent-up-down))

;; evil-textobj-line defines text objects for a single line.
(use-package evil-textobj-line
  :after evil)

;; Bound to "h"
(use-package evil-textobj-syntax
  ;; :straight (:host github :repo "laishulu/evil-textobj-syntax" :files ("*.el"))
  :after evil)

;; evil-args defines a new textobj for function arguments, and some other helpful functions.
(use-package evil-args
  :after evil
  :general
  (evil-inner-text-objects-map "," 'evil-inner-arg)
  (evil-outer-text-objects-map "," 'evil-outer-arg)
  (general-nmap "]," 'evil-forward-arg)
  (general-nmap "[," 'evil-backward-arg)
  (general-mmap "]," 'evil-forward-arg)
  (general-mmap "[," 'evil-backward-arg)
  (general-nmap "go" 'evil-jump-out-args))

(use-package evil-textobj-column
  :after evil
  ;; :straight (:host github :repo "noctuid/evil-textobj-column" :files ("*.el"))
  :general
  (evil-outer-text-objects-map
   "k" 'evil-textobj-column-word
   "K" 'evil-textobj-column-WORD))

;; evil motion through CamelCase words
(use-package evil-plugins
  :after evil
  ;; :straight (:host github :repo "tarao/evil-plugins" :files ("evil-little-word.el" "evil-textobj-between.el"))
  :config
  (require 'evil-little-world)
  (require 'evil-textobj-between)
  (define-key evil-normal-state-map (kbd "w") 'evil-forward-little-word-begin)
  (define-key evil-normal-state-map (kbd "b") 'evil-backward-little-word-begin)
  (define-key evil-operator-state-map (kbd "w") 'evil-forward-little-word-begin)
  (define-key evil-operator-state-map (kbd "b") 'evil-backward-little-word-begin)
  (define-key evil-visual-state-map (kbd "w") 'evil-forward-little-word-begin)
  (define-key evil-visual-state-map (kbd "b") 'evil-backward-little-word-begin)
  (define-key evil-visual-state-map (kbd "i w") 'evil-inner-little-word))

;; - https://github.com/gilbertw1/better-jumper
;; 另外一个 mode: https://github.com/overdr0ne/gumshoe
;; - https://www.reddit.com/r/emacs/comments/o8e6om/gumshoe_follows_you_around_and_logs_your_movements/
;; use better-jumper to replace evil jump backward< =C-i= > or forward< =C-o= >
;; (use-package better-jumper
;;   :custom
;;   ;; this is the key to avoiding conflict with evils jumping stuff
;;   (better-jumper-use-evil-jump-advice nil)
;;   :config
;;   (better-jumper-mode 1)

;;   ;; this lets me toggle between two points. (adapted from evil-jump-backward-swap)
;;   (evil-define-motion better-jumper-toggle (count)
;;     (let ((pnt (point)))
;;       (better-jumper-jump-backward)
;;       (better-jumper-set-jump pnt)))

;;   ;; this is the key here. This advice makes it so you only set a jump point
;;   ;; if you move more than one line with whatever command you call. For example
;;   ;; if you add this advice around evil-next-line, you will set a jump point
;;   ;; if you do 10 j, but not if you just hit j. I did not write this code, I
;;   ;; I found it a while back and updated it to work with better-jumper.
;;   (defun my-jump-advice (oldfun &rest args)
;;     (let ((old-pos (point)))
;;       (apply oldfun args)
;;       (when (> (abs (- (line-number-at-pos old-pos) (line-number-at-pos (point))))
;;                1)
;;         (save-excursion
;;           (goto-char old-pos)
;;           (better-jumper-set-jump)))))

;;   (with-eval-after-load 'evil-maps
;;     (define-key evil-motion-state-map (kbd "C-o") 'better-jumper-jump-backward)
;;     (define-key evil-motion-state-map (kbd "C-i") 'better-jumper-jump-forward))

;;   ;; jump scenarios
;;   (advice-add 'evil-next-line :around #'my-jump-advice)
;;   (advice-add 'evil-previous-line :around #'my-jump-advice)
;;   (advice-add 'swiper :around #'my-jump-advice)
;;   (advice-add 'evil-goto-definition :around #'my-jump-advice)
;;   (advice-add 'evil-goto-mark :around #'my-jump-advice))
;; ... whenever you want a new jump scenario just use the above pattern.

;; (use-package evil-pinyin
;;   :straight (:host github :repo "laishulu/evil-pinyin")
;;   :init
;;   (setq evil-pinyin-with-search-rule 'exclam) ; 'exclam 感叹号开启，即: /!hy 可以匹配“汉语”
;;   :config
;;   (evil-select-search-module 'evil-search-module 'evil-search)

;;   (global-evil-pinyin-mode t))

;; (use-package evil-nerd-commenter
;;   :after evil general
;;   :config
;;   (evilnc-default-hotkeys)
;;   :general
;;   (evil-inner-text-objects-map
;;    "c" 'evilnc-inner-comment)
;;   (evil-outer-text-objects-map
;;    "c" 'evilnc-outer-commenter)
;;   ;; Emacs key bindings
;;   ("M-;"   'evilnc-comment-or-uncomment-lines
;;    "C-c l" 'evilnc-quick-comment-or-uncomment-to-the-line
;;    ;; "C-c c" 'evilnc-copy-and-comment-lines   ; 这个与 org-capture 按键冲突
;;    ;; "C-c p" 'evilnc-comment-or-uncomment-paragraphs
;;    ))

;; (use-package evil-commentary
;;   :after evil
;;   :config
;;   (evil-commentary-mode))

;; 高亮 evil 要编辑选中的块
(use-package evil-goggles
  :after evil
  :config
  (setq evil-goggles-duration 0.15)
  (evil-goggles-mode))

;; 显示 evil search 匹配的词
;; - https://github.com/juanjux/evil-search-highlight-persist
;; (use-package evil-search-highlight-persist
;;   :after evil
;;   :init
;;   (global-evil-search-highlight-persist t)
;;   :config
;;   ;; To only display string whose length is greater than or equal to 3
;;   (setq evil-search-highlight-string-min-len 3))

;; - https://github.com/edkolev/evil-lion
;; evil-lion defines an alignment operator.
(use-package evil-lion
  :general
  (general-nvmap "ga" 'evil-lion-left)
  (general-nvmap "gA" 'evil-lion-right))

;; TODO: evil-better-visual-line
(use-package evil-traces
  :hook
  (after-init . evil-traces-mode)
  :config
  (evil-traces-use-diff-faces))

;; - https://github.com/redguardtoo/evil-matchit
;; *About:* 快速在匹配的代码块分隔符跳转，按键 =%= ，比如 html 里面同级的 ~<div>~ ~</div>~ 。
;; evil-matchit is the port of, well, matchit. It also provides two text objects, namely a% and i%.
(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode t))

;; 通过 =*= ， =#= 快速查找选中的代码
;; This allows me to easily start a * or # search from a visual selection.
(use-package evil-visualstar
  :after evil
  :config
  (global-evil-visualstar-mode t))

;; - https://github.com/emacs-evil/evil-surround
;; - https://github.com/casouri/isolate
;; 快速给选中的代码块加上引号或者括号，这个与 smartparens 有一些重复。
(use-package evil-surround
  :after evil
  :init
  (global-evil-surround-mode t))

;; - https://github.com/syohex/emacs-anzu
;; - https://github.com/syohex/emacs-evil-anzu
;; anzu 的 evil 扩展，用于快速替换文本
(use-package evil-anzu
  :after (evil anzu))

;; evil-multiedit
;; evil-mc
;; evil-mc-extras

;; evil-numbers takes c-a back (and can be mapped to different states!).
(use-package evil-numbers
  :after evil
  :general
  (general-nvmap "C-a" 'evil-numbers/inc-at-pt)
  (general-nvmap "C-S-a" 'evil-numbers/dec-at-pt))

;; evil-fringe-mark

;; TODO: evil-vimish-fold

;; 通过在 evil mode 里面绑定组合键调用命令
;; - https://github.com/emacsorphanage/key-chord
(use-package key-chord
  :after evil
  :init
  (key-chord-mode 1)
  :config
  ;; (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "jk" 'avy-goto-char)
  (key-chord-define evil-normal-state-map "jk" 'avy-goto-char))

(provide 'init-evil)
