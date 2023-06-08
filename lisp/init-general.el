;; init-general.el --- Initialize general settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; - leader key 主要参考了这个设置：https://github.com/yanghaoxie/emacs-dotfile#install-general
;; - https://sam217pa.github.io/2016/09/23/keybindings-strategies-in-emacs/
;; 

;;; Code

(require 'init-funcs)

(use-package general
  :demand t
  ;; :init
  ;; (general-def :states '(normal motion visual) "<SPC>" nil)
  ;; :config
  ;; Vim-like definitions
  ;; (with-eval-after-load 'evil
  ;;   (general-evil-setup t))

  ;; (general-create-definer yc/nonprefix-keys
  ;;   :keymaps 'override)

  ;; (general-create-definer yc/leader-keys
  ;;   :states '(normal motion visual insert emacs)
  ;;   :keymaps 'override
  ;;   :prefix "SPC"
  ;;   ;; :non-normal-prefix "s-SPC")
  ;;   :global-prefix "s-SPC")
  ;; ;; :prefix "C-c C-g")
  ;; ;; :keymaps 'override
  ;; ;; :prefix "<SPC>")
  ;; (general-create-definer yc/leader-keys-major-mode
  ;;   :states '(normal motion visual insert emacs)
  ;;   :keymaps 'override
  ;;   :prefix "SPC m"
  ;;   ;; :non-normal-prefix "s-SPC m")
  ;;   :global-prefix "s-SPC m")

  ;; (general-define-key
  ;;  :keymaps '(normal motion visual)
  ;;  "<f16>" (general-simulate-key "SPC m")
  ;; )

  ;; (general-create-definer my-leader-def-2
  ;;   :states '(normal motion visual)
  ;;   :global-prefix ",")

  ;; for frequently used prefix keys, the user can create a custom definer with a
  ;; default :prefix
  ;; using a variable is not necessary, but it may be useful if you want to
  ;; experiment with different prefix keys and aren't using `general-create-definer'
  ;; (defconst my-leader ",")

  ;; - https://github.com/noctuid/general.el
  ;; 参考这篇文章重新定义自己的 key bindings：https://leiyue.wordpress.com/2012/07/04/use-org-mode-and-taskjuggler-to-manage-to-project-information/
  ;; 参考在 Mac 下的一些配置：https://www.emacswiki.org/emacs/EmacsForMacOS
  ;; (yc/leader-keys
  ;;   "TAB"  'mode-line-other-buffer
  ;;   "SPC"  '(ivy-switch-buffer :which-key "Switch buffer")
  ;;   "C-g"  'keyboard-quit
  ;;   "?"    'which-key-show-top-level
  ;;   ;; "<"    '(counsel-projectile-find-file-dwim :which-key "Find file in project")
  ;;   "<"    '(counsel-fd-file-jump :which-key "Find file in project")
  ;;   ":"    '(counsel-M-x :which-key "M-x")
  ;;   ;; ";"    '(evil-ex :which-key "Ex command")
  ;;   "'"   '(multi-vterm-project :which-key "Open project shell")
  ;;   "\""    '(shell-pop :which-key "Open shell")
  ;;   "0"    '(treemacs-select-window :which-key "Treemacs")
  ;;   ;; "`"    'winum-select-window-0-or-10
  ;;   ;; "²"    'winum-select-window-by-number
  ;;   "1"    'delete-other-windows
  ;;   ;; "2"    'winum-select-window-2
  ;;   ;; "3"    'winum-select-window-3
  ;;   ;; "4"    'winum-select-window-4
  ;;   ;; "5"    'winum-select-window-5
  ;;   ;; "6"    'winum-select-window-6
  ;;   ;; "7"    'winum-select-window-7
  ;;   ;; "8"    'winum-select-window-8
  ;;   ;; "9"    'winum-select-window-9
  ;;   "/"     '(:ignore t :which-key "search")
  ;;   "//"    '(swiper :which-key "swiper")
  ;;   "/g"    'counsel-git-grep
  ;;   "-"    'yc/split-window-vertically
  ;;   "|"    'yc/split-window-horizontally
  ;;   "["    '(:ignore t :which-key "previous...")
  ;;   "[["   '(text-scale-increase :which-key "text size in")
  ;;   "]"    '(:ignore t :which-key "next...")
  ;;   "]]"   '(text-scale-decrease :which-key "text size out")
  ;;   "a"    '(:ignore t :which-key "applications")
  ;;   "ai"   '(:ignore t :which-key "irc")
  ;;   "as"   '(:ignore t :which-key "shells")
  ;;   "asa"  'shell-pop
  ;;   "ast"  'multi-vterm-project
  ;;   "asT"  'projectile-run-vterm
  ;;   "aR"   '(ranger :which-key "call ranger")
  ;;   "b"    '(:ignore t :which-key "buffers")
  ;;   ;; "bb"   'helm-mini
  ;;   "bk"   'kill-this-buffer
  ;;   "be"   'ibuffer
  ;;   "bb"   'bufler
  ;;   "c"    '(:ignore t :which-key "compile/comments")
  ;;   "C"    '(:ignore t :which-key "capture/colors")
  ;;   ;; "cl"   'evilnc-comment-or-uncomment-lines
  ;;   "e"    '(:ignore t :which-key "errors")
  ;;   "f"    '(:ignore t :which-key "files")
  ;;   "fC"   '(:ignore t :which-key "files/convert")
  ;;   "fe"   '(:ignore t :which-key "emacs(spacemacs)")
  ;;   "fv"   '(:ignore t :which-key "variables")
  ;;   ;; "fh"   'helm-find-files
  ;;   ;; "fc"   'helm-recentf
  ;;   "ff"   'counsel-find-file
  ;;   "fr"   'counsel-recentf
  ;;   "ft"   'treemacs
  ;;   "fs"   'save-buffer
  ;;   "fo"   'yc/dired-open-in-finder
  ;;   "F"    '(:ignore t :which-key "frame")
  ;;   "Fb"   'switch-to-buffer-other-frame
  ;;   "FD"   'delete-other-frames
  ;;   "Fd"   'delete-frame
  ;;   "Fn"   'make-frame
  ;;   "g"    '(:ignore t :which-key "git/versions-control")
  ;;   "gs"   '(magit-status :which-key "git status")
  ;;   "gf"   '(magit-log-buffer-file :wk "git log current file")
  ;;   "gl"   'magit-log
  ;;   "gb"   'magit-blame
  ;;   "g*"   'diff-hl-show-hunk
  ;;   "gt"   '(git-timemachine-toggle :which-key "git timemachine")
  ;;   ;; "gs"  'magit-status
  ;;   "h"    '(:ignore t :which-key "help")
  ;;   ;; "hb"   'helm-descbinds
  ;;   "hd"   '(:ignore t :which-key "help-describe")
  ;;   "hdk"  'describe-key
  ;;   "hdK"  'describe-keymap
  ;;   "hdb"  'describe-bindings
  ;;   "hdc"  'describe-char
  ;;   "hdv"  'describe-variable
  ;;   "hdm"  'describe-mode
  ;;   "hdl"  'describe-last-keys
  ;;   "hdf"  'describe-function
  ;;   "hk"   'which-key-show-top-level
  ;;   "hm"   'which-key-show-major-mode
  ;;   "i"    '(:ignore t :which-key "insertion")
  ;;   "j"    '(:ignore t :which-key "jump/join/split")
  ;;   "jj"   'avy-goto-word-or-subword-1
  ;;   "jk"   'avy-goto-char
  ;;   "jl"   'avy-goto-line
  ;;   "jb"   'counsel-bookmark
  ;;   "u"    'universal-argument
  ;;   "k"    '(:ignore t :which-key "lisp")
  ;;   "kd"   '(:ignore t :which-key "delete")
  ;;   "kD"   '(:ignore t :which-key "delete-backward")
  ;;   "k`"   '(:ignore t :which-key "hybrid")
  ;;   "n"    '(:ignore t :which-key "narrow/numbers")
  ;;   "q"    '(:ignore t :which-key "quit")
  ;;   "qs"   'save-buffers-kill-emacs
  ;;   "qq"   'delete-window
  ;;   "qr"   'restart-emacs
  ;;   "qQ"   'kill-emacs
  ;;   "r"    '(:ignore t :which-key "Undo/Redo/registers/rings/resume")
  ;;   "ru"   'undo-tree-visualize
  ;;   "m"    '(:ignore t :which-key "major-mode-cmd")
  ;;   "m."   'major-mode-hydra
  ;;   "Re"   '(:ignore t :which-key "elisp")
  ;;   "Rp"   '(:ignore t :which-key "pcre")
  ;;   "s"    '(:ignore t :which-key "search/symbol")
  ;;   "sa"   '(:ignore t :which-key "ag")
  ;;   "sg"   '(:ignore t :which-key "grep")
  ;;   "sk"   '(:ignore t :which-key "ack")
  ;;   "st"   '(:ignore t :which-key "pt")
  ;;   "sw"   '(:ignore t :which-key "web")
  ;;   "saa"  'ag
  ;;   "sar"  'rg
  ;;   "sap"  'ag-project
  ;;   "saA"  'counsel-ag
  ;;   "saR"  'counsel-rg
  ;;   "sad"  'deadgrep
  ;;   "sag"  'counsel-git-grep
  ;;   ;; "sas"  'helm-ag
  ;;   ;; "saf"  'helm-do-ag
  ;;   ;; "sap"  'helm-do-ag-project-root
  ;;   ;; "sc"   'evil-search-highlight-persist-remove-all
  ;;   ;; "ss"   'helm-swoop
  ;;   ;; "sS"   'helm-swoop-region-or-symbol
  ;;   ;; "mH"   'hydra-move-splitter-left
  ;;   ;; "mJ"   'hydra-move-splitter-down
  ;;   ;; "mK"   'hydra-move-splitter-up
  ;;   ;; "mL"   'hydra-move-splitter-right
  ;;   ;; "mM"   'delete-other-windows
  ;;   ;; "mb"   'balance-windows
  ;;   "t"    '(:ignore t :which-key "toggles")
  ;;   "tC"   '(:ignore t :which-key "colors")
  ;;   "tE"   '(:ignore t :which-key "editing-styles")
  ;;   "th"   '(:ignore t :which-key "highlight")
  ;;   ;; "tm"   '(:ignore t :which-key "modeline")
  ;;   "t."   'hydra-toggles/body
  ;;   "tf"   'toggle-frame-fullscreen
  ;;   "tm"   'toggle-frame-maximized
  ;;   "ti"   'org-toggle-inline-images
  ;;   "to"   'org-sticky-header-mode
  ;;   "tt"   'centaur-tabs-mode
  ;;   "tc"   'blink-cursor-mode
  ;;   "x"    '(:ignore t :which-key "text")
  ;;   "xa"   '(:ignore t :which-key "align")
  ;;   "xd"   '(:ignore t :which-key "delete")
  ;;   "xg"   '(:ignore t :which-key "google-translate")
  ;;   "xl"   '(:ignore t :which-key "lines")
  ;;   "xm"   '(:ignore t :which-key "move")
  ;;   "xt"   '(:ignore t :which-key "transpose")
  
  ;;   "z"    '(:ignore t :which-key "zoom")
  ;;   "C-t"  '(:ignore t :which-key "other toggles")
  ;;   "o"    '(:ignore t :which-key "org")
  ;;   "o."   'hydra-org/body
  ;;   "oa"   'org-agenda
  
  ;;   "og"   'org-clock-goto
  ;;   "ok"   '(:ignore t :which-key "clock")
  ;;   "oki"  'org-clock-in
  ;;   "oko"  'org-clock-out
  ;;   "okc"  'org-clock-in-last
  ;;   "oke"  'org-clock-modify-effort-estimate
  ;;   "okq"  'org-clock-cancel
  ;;   "okd"  'org-clock-display
  ;;   "okg"  'org-clock-goto
  ;;   "okr"  'org-clock-report
  ;;   "ao"   '(:ignore t :which-key "org")
  ;;   "aoa"  'org-agenda
  ;;   "aoc"  'org-capture
  ;;   "aok"  '(:ignore t :which-key "clock")
  ;;   "aoki" 'org-clock-in
  ;;   "aoko" 'org-clock-out
  ;;   "aokc" 'org-clock-in-last
  ;;   "aoke" 'org-clock-modify-effort-estimate
  ;;   "aokq" 'org-clock-cancel
  ;;   "aokg" 'org-clock-goto
  ;;   "aokd" 'org-clock-display
  ;;   "aokr" 'org-clock-report
  ;;   "v"    'er/expand-region)
  )

;; (general-nmap
;;   :prefix my-leader
;;   "1"  'delete-other-windows
;;   "0"  'delete-window
;;   "q"  'delete-window
;;   "f"  'counsel-find-file
;;   "b"  'switch-to-buffer)

;; (general-nmap
;;   :prefix my-leader
;;   "c"  '(:ignore t :which-key "comment or uncomment")
;;   "ci" 'evilnc-comment-or-uncomment-lines
;;   "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
;;   "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
;;   "cc" 'evilnc-copy-and-comment-lines
;;   "cp" 'evilnc-comment-or-uncomment-paragraphs
;;   "cr" 'comment-or-uncomment-region
;;   "cv" 'evilnc-toggle-invert-comment-line-by-line
;;   "\\" 'evilnc-comment-operator    ; if you prefer backslash key
;;   )

;; (general-create-definer yc/leader-keys-major-mode
;;   :states '(normal motion visual)
;;   :prefix "\\")

(global-set-key (kbd "C-x 2") 'yc/split-window-vertically)
(global-set-key (kbd "C-x 3") 'yc/split-window-horizontally)

(general-define-key "C-c n n" 'yc/new-buffer-frame)
(general-define-key "C-x C-m" 'yc/move-file)

(provide 'init-general)
