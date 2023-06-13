;; init-general.el --- Initialize general settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; - leader key 主要参考了这个设置：https://github.com/yanghaoxie/emacs-dotfile#install-general
;; - https://sam217pa.github.io/2016/09/23/keybindings-strategies-in-emacs/
;; 

;;; Code:
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
  ;;   "SPC"  '(ivy-switch-buffer :wk "Switch buffer")
  ;;   "C-g"  'keyboard-quit
  ;;   "?"    'which-key-show-top-level
  ;;   ;; "<"    '(counsel-projectile-find-file-dwim :wk "Find file in project")
  ;;   "<"    '(counsel-fd-file-jump :wk "Find file in project")
  ;;   ":"    '(counsel-M-x :wk "M-x")
  ;;   ;; ";"    '(evil-ex :wk "Ex command")
  ;;   "'"   '(multi-vterm-project :wk "Open project shell")
  ;;   "\""    '(shell-pop :wk "Open shell")
  ;;   "0"    '(treemacs-select-window :wk "Treemacs")
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
  ;;   "/"     '(:ignore t :wk "search")
  ;;   "//"    '(swiper :wk "swiper")
  ;;   "/g"    'counsel-git-grep
  ;;   "-"    'yc/split-window-vertically
  ;;   "|"    'yc/split-window-horizontally
  ;;   "["    '(:ignore t :wk "previous...")
  ;;   "[["   '(text-scale-increase :wk "text size in")
  ;;   "]"    '(:ignore t :wk "next...")
  ;;   "]]"   '(text-scale-decrease :wk "text size out")
  ;;   "a"    '(:ignore t :wk "applications")
  ;;   "ai"   '(:ignore t :wk "irc")
  ;;   "as"   '(:ignore t :wk "shells")
  ;;   "asa"  'shell-pop
  ;;   "ast"  'multi-vterm-project
  ;;   "asT"  'projectile-run-vterm
  ;;   "aR"   '(ranger :wk "call ranger")
  ;;   "b"    '(:ignore t :wk "buffers")
  ;;   ;; "bb"   'helm-mini
  ;;   "bk"   'kill-this-buffer
  ;;   "be"   'ibuffer
  ;;   "bb"   'bufler
  ;;   "c"    '(:ignore t :wk "compile/comments")
  ;;   "C"    '(:ignore t :wk "capture/colors")
  ;;   ;; "cl"   'evilnc-comment-or-uncomment-lines
  ;;   "e"    '(:ignore t :wk "errors")
  ;;   "f"    '(:ignore t :wk "files")
  ;;   "fC"   '(:ignore t :wk "files/convert")
  ;;   "fe"   '(:ignore t :wk "emacs(spacemacs)")
  ;;   "fv"   '(:ignore t :wk "variables")
  ;;   ;; "fh"   'helm-find-files
  ;;   ;; "fc"   'helm-recentf
  ;;   "ff"   'counsel-find-file
  ;;   "fr"   'counsel-recentf
  ;;   "ft"   'treemacs
  ;;   "fs"   'save-buffer
  ;;   "fo"   'yc/dired-open-in-finder
  ;;   "F"    '(:ignore t :wk "frame")
  ;;   "Fb"   'switch-to-buffer-other-frame
  ;;   "FD"   'delete-other-frames
  ;;   "Fd"   'delete-frame
  ;;   "Fn"   'make-frame
  ;;   "g"    '(:ignore t :wk "git/versions-control")
  ;;   "gs"   '(magit-status :wk "git status")
  ;;   "gf"   '(magit-log-buffer-file :wk "git log current file")
  ;;   "gl"   'magit-log
  ;;   "gb"   'magit-blame
  ;;   "g*"   'diff-hl-show-hunk
  ;;   "gt"   '(git-timemachine-toggle :wk "git timemachine")
  ;;   ;; "gs"  'magit-status
  ;;   "h"    '(:ignore t :wk "help")
  ;;   ;; "hb"   'helm-descbinds
  ;;   "hd"   '(:ignore t :wk "help-describe")
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
  ;;   "i"    '(:ignore t :wk "insertion")
  ;;   "j"    '(:ignore t :wk "jump/join/split")
  ;;   "jj"   'avy-goto-word-or-subword-1
  ;;   "jk"   'avy-goto-char
  ;;   "jl"   'avy-goto-line
  ;;   "jb"   'counsel-bookmark
  ;;   "u"    'universal-argument
  ;;   "k"    '(:ignore t :wk "lisp")
  ;;   "kd"   '(:ignore t :wk "delete")
  ;;   "kD"   '(:ignore t :wk "delete-backward")
  ;;   "k`"   '(:ignore t :wk "hybrid")
  ;;   "n"    '(:ignore t :wk "narrow/numbers")
  ;;   "q"    '(:ignore t :wk "quit")
  ;;   "qs"   'save-buffers-kill-emacs
  ;;   "qq"   'delete-window
  ;;   "qr"   'restart-emacs
  ;;   "qQ"   'kill-emacs
  ;;   "r"    '(:ignore t :wk "Undo/Redo/registers/rings/resume")
  ;;   "ru"   'undo-tree-visualize
  ;;   "m"    '(:ignore t :wk "major-mode-cmd")
  ;;   "m."   'major-mode-hydra
  ;;   "Re"   '(:ignore t :wk "elisp")
  ;;   "Rp"   '(:ignore t :wk "pcre")
  ;;   "s"    '(:ignore t :wk "search/symbol")
  ;;   "sa"   '(:ignore t :wk "ag")
  ;;   "sg"   '(:ignore t :wk "grep")
  ;;   "sk"   '(:ignore t :wk "ack")
  ;;   "st"   '(:ignore t :wk "pt")
  ;;   "sw"   '(:ignore t :wk "web")
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
  ;;   "t"    '(:ignore t :wk "toggles")
  ;;   "tC"   '(:ignore t :wk "colors")
  ;;   "tE"   '(:ignore t :wk "editing-styles")
  ;;   "th"   '(:ignore t :wk "highlight")
  ;;   ;; "tm"   '(:ignore t :wk "modeline")
  ;;   "t."   'hydra-toggles/body
  ;;   "tf"   'toggle-frame-fullscreen
  ;;   "tm"   'toggle-frame-maximized
  ;;   "ti"   'org-toggle-inline-images
  ;;   "to"   'org-sticky-header-mode
  ;;   "tt"   'centaur-tabs-mode
  ;;   "tc"   'blink-cursor-mode
  ;;   "x"    '(:ignore t :wk "text")
  ;;   "xa"   '(:ignore t :wk "align")
  ;;   "xd"   '(:ignore t :wk "delete")
  ;;   "xg"   '(:ignore t :wk "google-translate")
  ;;   "xl"   '(:ignore t :wk "lines")
  ;;   "xm"   '(:ignore t :wk "move")
  ;;   "xt"   '(:ignore t :wk "transpose")
  
  ;;   "z"    '(:ignore t :wk "zoom")
  ;;   "C-t"  '(:ignore t :wk "other toggles")
  ;;   "o"    '(:ignore t :wk "org")
  ;;   "o."   'hydra-org/body
  ;;   "oa"   'org-agenda
  
  ;;   "og"   'org-clock-goto
  ;;   "ok"   '(:ignore t :wk "clock")
  ;;   "oki"  'org-clock-in
  ;;   "oko"  'org-clock-out
  ;;   "okc"  'org-clock-in-last
  ;;   "oke"  'org-clock-modify-effort-estimate
  ;;   "okq"  'org-clock-cancel
  ;;   "okd"  'org-clock-display
  ;;   "okg"  'org-clock-goto
  ;;   "okr"  'org-clock-report
  ;;   "ao"   '(:ignore t :wk "org")
  ;;   "aoa"  'org-agenda
  ;;   "aoc"  'org-capture
  ;;   "aok"  '(:ignore t :wk "clock")
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
;;   "c"  '(:ignore t :wk "comment or uncomment")
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
;;; init-general.el ends here
