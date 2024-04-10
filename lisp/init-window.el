;; init-window.el --- Initialize window settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:
(require 'init-funcs)

(use-package pixel-scroll
  :ensure nil
  :bind
  (([remap scroll-up-command]    . +pixel-scroll-up-command)
   ([remap scroll-down-command]  . +pixel-scroll-down-command)
   ([remap recenter-top-bottom]  . +pixel-recenter-top-bottom)
   ("C-M-v"   . sloth/scroll-other-window-up)
   ("C-M-S-v" . sloth/scroll-other-window-down))
  :custom
  (pixel-scroll-precision-interpolation-factor 1.0)
  (pixel-scroll-precision-interpolate-page t)
  :init
  (pixel-scroll-precision-mode t)
  :config
  ;; scroll less than default
  (defvar fk/default-scroll-lines 15)
  
  (defun +pixel-scroll-up-command ()
    "Similar to `scroll-up-command' but with pixel scrolling."
    (interactive)
    (pixel-scroll-precision-interpolate (- (* fk/default-scroll-lines (line-pixel-height)))))

  (defun +pixel-scroll-down-command ()
    "Similar to `scroll-down-command' but with pixel scrolling."
    (interactive)
    (pixel-scroll-precision-interpolate (* fk/default-scroll-lines (line-pixel-height))))

  (defun +pixel-recenter-top-bottom ()
    "Similar to `recenter-top-bottom' but with pixel scrolling."
    (interactive)
    (let* ((current-row (cdr (nth 6 (posn-at-point))))
           (target-row (save-window-excursion
                         (recenter-top-bottom)
                         (cdr (nth 6 (posn-at-point)))))
           (distance-in-pixels (* (- target-row current-row) (line-pixel-height))))
      (pixel-scroll-precision-interpolate distance-in-pixels)))

  (defun sloth/scroll-other-window-up ()
    "Scroll selected window up."
    (interactive)
    (when current-prefix-arg
      (setq-local other-window-scroll-buffer
                  (window-buffer (aw-select "Select window to scroll"))))
    (scroll-other-window))

  (defun sloth/scroll-other-window-down ()
    "Scroll selected window down."
    (interactive)
    (when current-prefix-arg
      (setq-local other-window-scroll-buffer
                  (window-buffer (aw-select "Select window to scroll"))))
    (scroll-other-window-down)))

(use-package window
  :ensure nil
  :general
  ("s-<left>"     'hydra-move-splitter-left
   "s-<right>"    'hydra-move-splitter-right
   "s-<up>"       'hydra-move-splitter-up
   "s-<down>"     'hydra-move-splitter-down
   "s-S-<left>"   'hydra-move-splitter-left-4x
   "s-S-<right>"  'hydra-move-splitter-right-4x
   "s-<return>"   'doom/window-enlargen))

;; Winner Mode 是 Emacs 自带的一个 minor mode，可以用于快速恢复窗口分割状态。
;; 默认使用 =C-c <left>= 组合键，就可以快速退回上一个窗口设置； =C-c <right>= 组合键，向前恢复一个窗口设置。

;; 在 Hydra 模式下， =u= 按键快速回退上一个窗口； =r= 按键快速向前恢复一个窗口。

;; winner-mode 是一个全局的 minor mode，它的主要功能是记录窗体的变动。例如当前有 2 个窗口，然后你关了一个，这时可以通过
;; winner-undo 来恢复。还可以再 winner-redo 来撤销刚才的 undo。
(use-package winner-mode
  :ensure nil
  :hook (after-init . winner-mode)
  :config
  ;; Add advice to stop hangs on EXWM
  ;; The problem happens with floating windows that disappear - like open file dialog or a Zoom dialog when starting a meeting
  ;; The solution is to assure all frames in winner-modified-list pass the frame-live-p test
  (defun gjg/winner-clean-up-modified-list ()
    "Remove dead frames from `winner-modified-list`"
    (dolist (frame winner-modified-list)
      (unless (frame-live-p frame)
        (delete frame winner-modified-list))))
  (advice-add 'winner-save-old-configurations :before #'gjg/winner-clean-up-modified-list))

(use-package winum
  :general
  (winum-keymap
   ;; "M-0" 'winum-select-window-0-or-10
   "M-1" 'winum-select-window-1
   "M-2" 'winum-select-window-2
   "M-3" 'winum-select-window-3
   "M-4" 'winum-select-window-4
   "M-5" 'winum-select-window-5
   "M-6" 'winum-select-window-6
   "M-7" 'winum-select-window-7
   "M-8" 'winum-select-window-8
   "M-9" 'winum-select-window-9)
  :init
  (setq winum-auto-setup-mode-line nil)
  (winum-mode 1)
  :config
  (setq winum-auto-assign-0-to-minibuffer nil
        winum-ignored-buffers '(" *which-key*")))

;; 或许试试这个 Package： https://github.com/dimitri/switch-window
;; https://sachachua.com/blog/2015/01/emacs-microhabit-switching-windows-windmove-ace-window-ace-jump/
(use-package ace-window
  :general
  ("M-o" 'ace-window)
  :init
  (setq ;; aw-keys '(?h ?j ?k ?l ?y ?u ?i ?o ?p)
   aw-background nil
   aw-scope 'frame
   aw-dispatch-alist
   '((?s aw-swap-window "swap window")
     (?2 aw-split-window-vert "split window vertically")
     (?3 aw-split-window-horz "split window horizontally")
     (?x aw-delete-window "delete window")
     (?? aw-show-dispatch-help)))
  :custom-face
  (aw-leading-char-face ((t (:inherit font-lock-keyword-face :foreground unspecified :bold t :height 3.0))))
  (aw-minibuffer-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 1.0))))
  (aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))))
  :config
  (ace-window-display-mode 1))

;; 类似于 tmux 的最大化当前窗口功能，保持和我在 tmux 下的习惯一致。
;; https://github.com/syohex/emacs-zoom-window
(use-package zoom-window
  :commands zoom-window-zoom
  :general
  ("C-x w z" 'zoom-window-zoom)
  :config
  (setq zoom-window-mode-line-color "DarkGreen"))

(use-package rotate
  ;; :commands (rotate-layout rotate-window hydra-rotate-window/body)
  :general
  ("C-x w ." 'hydra-rotate-window/body)
  :init
  (defhydra hydra-rotate-window ()
    "rotate-layout"
    ("SPC" rotate-layout "rotate")
    ("w" rotate-window "swap")
    ("1" delete-other-windows "maximize" :exit t) ; 暂时不加这个，因为旋转窗口就是因为有多窗口的需要
    ("j" (progn (scroll-down-line 1)) "↓")
    ("J" (progn (scroll-down-line 4)) "4x ↓")
    ("k" (progn (scroll-up-line 1)) "↑")
    ("K" (progn (scroll-up-line 4)) "4x ↑")
    ("C-g" nil "quit")
    ("q" nil "quit")))

;; - https://depp.brause.cc/shackle/
;; - file:~/src/emacs.d/kaushalmodi-emacs.d/setup-files/setup-shackle.el
;; - file:~/src/emacs.d/seagle0128.emacs.d/lisp/init-window.el
;; - file:~/src/emacs.d/Fuco1-emacd.d/files/vendor.el
;; - file:~/src/emacs.d/redguardtoo-emacs.d/lisp/init-shackle.el
;; - file:~/src/emacs.d/john-jwiegley-dot-emacs/settings.el
;; Enforce rules for popups
(defvar shackle--popup-window-list nil) ; all popup windows
(defvar-local shackle--current-popup-window nil) ; current popup window
(put 'shackle--current-popup-window 'permanent-local t)

;; https://github.com/dustinlacewell/hera
(defvar jp-window--title (with-faicon "nf-fa-window_restore" "Window Management" 1 -0.05))
(pretty-hydra-define hydra-window
  (:hint nil :foreign-keys warn :quit-key "q" :title jp-window--title :separator "═")
  (;; general window management commands
   "Windows"
   (("x" ace-delete-window "delete")
    ("s" ace-swap-window "swap")
    ("a" ace-select-window "select")
    ("o" other-window "cycle")
    ("d" delete-window "delete")
    ("m" ace-delete-other-windows "maximize")
    ("M" delete-other-windows "delete other windows")
    ;;("K" ace-delete-other-windows)
    ("S" save-buffer "Save Buffer")
    ("D" (lambda ()
           (interactive)
           (ace-delete-window)
           (add-hook 'ace-window-end-once-hook
                     'hydra-window/body)) "delete"))
   ;; resize
   "Resize"
   (("h" hydra-move-splitter-left "←")
    ("j" hydra-move-splitter-down "↓")
    ("k" hydra-move-splitter-up "↑")
    ("l" hydra-move-splitter-right "→")
    ("n" balance-windows "balance")
    ("H" hydra-move-splitter-left-4x "←")
    ("J" enlarge-window "↓")
    ("K" shrink-window "↑")
    ("L" hydra-move-splitter-right-4x "→"))
   ;; split
   "Split"
   (("b" split-window-right "horizontally")
    ("B" split-window-horizontally-instead "horizontally instead")
    ("v" split-window-below "vertically")
    ("V" split-window-vertically-instead "vertically instead")
    ("-" yc/split-window-horizontally "horizontally")
    ("|" yc/split-window-vertically "vertically")
    ("u" (progn
           (winner-undo)
           (setq this-command 'winner-undo)) "undo")
    ("r" winner-redo "redo"))
   "Zoom"
   (("+" text-scale-increase "in")
    ("-" text-scale-decrease "out")
    ;; ("0" (text-scale-set 0) "reset")
    ("0" (text-scale-adjust 0) "reset"))
   "Eyebrowse"
   (("<" eyebrowse-prev-window-config "previous")
    (">" eyebrowse-next-window-config "next")
    ("C" eyebrowse-create-window-config "create")
    ("E" eyebrowse-last-window-config "last")
    ("K" eyebrowse-close-window-config "kill")
    ("R" eyebrowse-rename-window-config "rename")
    ("w" eyebrowse-switch-to-window-config "switch")
    ("1" eyebrowse-switch-to-window-config-1 "workspace ➊")
    ("2" eyebrowse-switch-to-window-config-2 "workspace ➋")
    ("3" eyebrowse-switch-to-window-config-3 "workspace ➌")
    ("4" eyebrowse-switch-to-window-config-4 "workspace ➍"))
   ;; ;; Move
   ;; "Movement" (("h" windmove-left)
   ;;             ("j" windmove-down)
   ;;             ("k" windmove-up)
   ;;             ("l" windmove-right)
   ;;             )

   ;; "Window Purpose" (("P" purpose-set-window-purpose)
   ;;                   ("B" ivy-purpose-switch-buffer-with-purpose)
   ;;                   ("!" purpose-toggle-window-purpose-dedicated)
   ;;                   ("#" purpose-toggle-window-buffer-dedicated))
   ;; "Others" (
   ;;           ("x" counsel-M-x)
   ;;           ("q" nil))
   "Switch"
   (("b" ivy-purpose-switch-buffer-without-purpose)
    ("f" counsel-find-file "find file")
    ("a" (lambda ()
           (interactive)
           (ace-window 1)
           (add-hook 'ace-window-end-once-hook
                     'hydra-window/body)) "switch")
    ("s" (lambda ()
           (interactive)
           (ace-swap-window)
           (add-hook 'ace-window-end-once-hook
                     'hydra-window/body)) "swap"))))

(use-package shackle
  :general
  ("C-c z" 'shackle-last-popup-buffer)
  :functions org-switch-to-buffer-other-window
  :commands shackle-display-buffer
  :hook (after-init . shackle-mode)
  :config
  (eval-and-compile
    (defun shackle-last-popup-buffer ()
      "View last popup buffer."
      (interactive)
      (ignore-errors
        (display-buffer shackle-last-buffer)))

    ;; Add keyword: `autoclose'
    (defun shackle-display-buffer-hack (fn buffer alist plist)
      (let ((window (funcall fn buffer alist plist)))
        (setq shackle--current-popup-window window)

        (when (plist-get plist :autoclose)
          (push (cons window buffer) shackle--popup-window-list))
        window))

    (defun shackle-close-popup-window-hack (&rest _)
      "Close current popup window via `C-g'."
      (setq shackle--popup-window-list
            (cl-loop for (window . buffer) in shackle--popup-window-list
                     if (and (window-live-p window)
                             (equal (window-buffer window) buffer))
                     collect (cons window buffer)))
      ;; `C-g' can deactivate region
      (when (and (called-interactively-p 'interactive)
                 (not (region-active-p)))
        (let (window buffer)
          (if (one-window-p)
              (progn
                (setq window (selected-window))
                (when (equal (buffer-local-value 'shackle--current-popup-window
                                                 (window-buffer window))
                             window)
                  (winner-undo)))
            (setq window (caar shackle--popup-window-list))
            (setq buffer (cdar shackle--popup-window-list))
            (when (and (window-live-p window)
                       (equal (window-buffer window) buffer))
              (delete-window window)

              (pop shackle--popup-window-list))))))

    (advice-add #'keyboard-quit :before #'shackle-close-popup-window-hack)
    (advice-add #'shackle-display-buffer :around #'shackle-display-buffer-hack))

  ;; HACK: compatibility issuw with `org-switch-to-buffer-other-window'
  (advice-add #'org-switch-to-buffer-other-window :override #'switch-to-buffer-other-window)

  ;; rules
  (setq shackle-default-size 0.4
        shackle-default-alignment 'below
        shackle-default-rule nil
        shackle-rules
        '((("*Help*" "*Apropos*") :select t :size 0.3 :align 'below :autoclose t)
          (compilation-mode :select t :size 0.3 :align 'below :autoclose t)
          ("*Completions*" :size 0.3 :align 'below :autoclose t)
          ("*Pp Eval Output*" :size 15 :align 'below :autoclose t)
          ("*Backtrace*" :select t :size 15 :align 'below)
          (("*Warnings*" "*Messages*") :size 0.3 :align 'below :autoclose t)
          ("^\\*.*Shell Command.*\\*$" :regexp t :size 0.3 :align 'below :autoclose t)
          ;; ("^\\*vterminal-.*\\*$" :regexp t :size 0.3 :align 'below :autoclose t)
          ;; ("^\\*vterminal -.*\\*$" :regexp t :size 0.3 :align 'below :autoclose t)
          ;; ("^\\*vterminal<.+>\\*$" :regexp t :size 0.3 :align 'below :autoclose t)
          ;; ("^\\*vterm - .*\\*$" :regexp t :size 0.3 :align 'below :autoclose t)
          ;; ("^\\*vterm .*\\*$" :regexp t :size 0.3 :align 'below :autoclose t)
          (vterm-mode :select t :size 0.4 :align 'below :autoclose t)
          ("\\*[Wo]*Man.*\\*" :regexp t :select t :align 'below :autoclose t)
          ("*Calendar*" :select t :size 0.3 :align 'below)
          (("*shell*" "*eshell*" "*ielm*") :popup t :size 0.3 :align 'below)
          ("^\\*vc-.*\\*$" :regexp t :size 0.3 :align 'below :autoclose t)
          ("*gud-debug*" :select t :size 0.4 :align 'below :autoclose t)
          ("\\*ivy-occur .*\\*" :regexp t :select t :size 0.3 :align 'below)
          (" *undo-tree*" :select t)
          ("*quickrun*" :select t :size 15 :align 'below)
          ("*tldr*" :size 0.4 :align 'below :autoclose t)
          ("*Youdao Dictionary*" :size 15 :align 'below :autoclose t)
          ("*Finder*" :select t :size 0.3 :align 'below :autoclose t)
          ("^\\*macro expansion\\**" :regexp t :size 0.4 :align 'below)
          ("^\\*elfeed-entry" :regexp t :size 0.7 :align 'below :autoclose t)
          ((" *Org todo*" "*Org Dashboard*" "*Org Select*") :select t :size 0.4 :align 'below :autoclose t)
          (" *Install vterm" :size 0.35 :same t :align 'below)
          (("*Paradox Report*" "*package update results*") :size 0.2 :align 'below :autoclose t)
          ("*Package-Lint*" :size 0.4 :align 'below :autoclose t)
          (("*Gofmt Errors*" "*Go Test*") :select t :size 0.3 :align 'below :autoclose t)
          ("*How Do You*" :select t :size 0.5 :align 'below :autoclose t)

          ("*ert*" :size 15 :align 'below :autoclose t)
          (overseer-buffer-mode :size 15 :align 'below :autoclose t)

          (" *Flycheck checkers*" :select t :size 0.3 :align 'below :autoclose t)
          ((flycheck-error-list-mode flymake-diagnostics-buffer-mode)
           :select t :size 0.25 :align 'below :autoclose t)

          (("*lsp-help*" "*lsp session*") :size 0.3 :align 'below :autoclose t)
          ("*DAP Templates*" :select t :size 0.4 :align 'below :autoclose t)
          (dap-server-log-mode :size 15 :align 'below :autoclose t)
          ("*rustfmt*" :select t :size 0.3 :align 'below :autoclose t)

          (profiler-report-mode :select t :size 0.5 :align 'below)
          ("*ELP Profiling Restuls*" :select t :size 0.5 :align 'below)

          ((inferior-python-mode inf-ruby-mode swift-repl-mode) :size 0.4 :align 'below)
          ("*prolog*" :size 0.4 :align 'below)

          ((grep-mode rg-mode deadgrep-mode ag-mode pt-mode) :select t :size 0.4 :align 'below)
          (Buffer-menu-mode :select t :size 20 :align 'below :autoclose t)
          (gnus-article-mode :select t :size 0.7 :align 'below :autoclose t)
          (helpful-mode :select t :size 0.3 :align 'below :autoclose t)
          ((process-menu-mode cargo-process-mode) :select t :size 0.3 :align 'below :autoclose t)
          (list-environment-mode :select t :size 0.3 :align 'below :autoclose t)
          (tabulated-list-mode :size 0.4 :align 'below))))

(use-package good-scroll
  :disabled t
  :config
  (good-scroll-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  scrolling                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This makes scrolling gradual rather than by half page.
;; I find that the half page scroll really makes me lose where I am
;; in the file so here I make sure to scroll one line at a time.
;; In addition I want to keep what I'm working on centered so
;; I start scrolling when the cursor is 10 lines away from the margin.
;; This behaviour in general emulates the scrolloff option in vim.
;; faster scrolling
;; - https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag/28746
;; - https://github.com/wandersoncferreira/dotfiles/blob/master/README.org#defaults

;; ;; (setq scroll-margin 10
;; ;;       scroll-step 1
;; ;;       scroll-conservatively 10000
;; ;;       scroll-preserve-screen-position 1)
;; (setq  hscroll-margin 1
;;        hscroll-step 1
;;        scroll-conservatively 100000
;;        scroll-margin 0
;;        scroll-preserve-screen-position t)
;; (setq auto-window-vscroll nil)

;; (use-package yascroll
;;   :init
;;   (global-yascroll-bar-mode t))

;; (use-package smooth-scrolling
;;   :init
;;   (smooth-scrolling-mode t)
;;   (setq smooth-scroll-margin 5))

;; (use-package scrollkeeper
;;   :bind (([remap scroll-up-command] . scrollkeeper-contents-up)
;;          ([remap scroll-down-command] . scrollkeeper-contents-down)))

;; (use-package sublimity
;;   :commands (sublimity-map-show sublimity-mode)
;;   :config
;;   (require 'sublimity)
;;   (require 'sublimity-scroll)
;;   (require 'sublimity-map)
;;   (setq sublimity-scroll-weight 5
;;         sublimity-scroll-drift-length 10
;;         sublimity-map-size 20
;;         sublimity-map-fraction 0.3
;;         sublimity-map-text-scale -7)
;;   (setq sublimity-map-set-delay 5)
;;   (setq sublimity-attractive-hide-bars t)
;;   (setq sublimity-attractive-hide-vertical-border t)
;;   (setq sublimity-attractive-hide-fringes t)
;;   (setq sublimity-attractive-hide-modelines t))

;; eyebrowse 是一个类似 i3wm 的平铺窗口管理器，可以设置多个工作空间。
;; 目前是使用 =<f5>= 、 =<f6>= 、 =<f7>= 、 =<f8>= 进行工作空间切换。
;; *注意：* 这个的使用和 Winner Mode 有点冲突，通过 Winner Mode 进行恢复窗口时，会恢复到其他工作空间的窗口。
;; https://github.com/wasamasa/eyebrowse
;; - TODO: 把这个 eyebrowse 的按键配置到 Hydra 上面。
(use-package eyebrowse
  :disabled t
  :init
  (eyebrowse-mode 1)
  :config
  (setq-default eyebrowse-new-workspace t)

  ;; (defhydra hydra-eyebrowse (:color blue)
  ;;   "
  ;; ^
  ;; ^Eyebrowse^         ^Do^                ^Switch^
  ;; ^─────────^─────────^──^────────────────^──────^────────────
  ;; _q_ quit            _c_ create          _<_ previous
  ;; ^^                  _k_ kill            _>_ next
  ;; ^^                  _r_ rename          _e_ last
  ;; ^^                  ^^                  _s_ switch
  ;; ^^                  ^^                  ^^
  ;; "
  ;;   ("q" nil)
  ;;   ("<" eyebrowse-prev-window-config :color red)
  ;;   (">" eyebrowse-next-window-config :color red)
  ;;   ("c" eyebrowse-create-window-config)
  ;;   ("e" eyebrowse-last-window-config)
  ;;   ("k" eyebrowse-close-window-config :color red)
  ;;   ("r" eyebrowse-rename-window-config)
  ;;   ("s" eyebrowse-switch-to-window-config))
  )

;; - https://github.com/pashinin/workgroups2
(use-package workgroups2
  :disabled t
  :config
  ;; Change prefix key (before activating WG)
  ;; (setq wg-prefix-key (kbd "C-c z"))

  ;; Change workgroups session file
  (setq wg-session-file (expand-file-name ".cache/.emacs_workgroups" user-emacs-directory))
  ;; What to do on Emacs exit / workgroups-mode exit?
  (setq wg-emacs-exit-save-behavior 'save)          ; Options: 'save 'ask nil
  (setq wg-workgroups-mode-exit-save-behavior 'save) ; Options: 'save 'ask nil

  ;; Mode Line changes
  ;; Display workgroups in Mode Line?
  (setq wg-mode-line-display-on t)      ; Default: (not (featurep 'powerline))
  (setq wg-flag-modified t)             ; Display modified flags as well
  (setq wg-mode-line-decor-left-brace "["
        wg-mode-line-decor-right-brace "]" ; how to surround it
        wg-mode-line-decor-divider ":")
  (workgroups-mode 1))

;; - https://github.com/alphapapa/burly.el
(use-package burly
  :disabled t)

;; 透明化窗口设置
(use-package transwin
  :disabled t
  ;; :straight (:host github :repo "jcs-elpa/transwin")
  :config
  (transwin-toggle-transparent-frame))

;; (defhydra hydra-window ()
;;   "
;;     Movement^   ^Split^         ^Switch^       ^^^Resize^         ^Window Purpose^
;;     ------------------------------------------------------------------------------------------------------
;;     _h_ ←        _|_ vertical    ^_b_uffer       _H_  X←          choose window _P_urpose
;;     _j_ ↓        _-_ horizontal  ^_f_ind files   _J_  X↓          switch to _B_uffer w/ same purpose
;;     _k_ ↑        _u_ undo        ^_a_ce window   _K_  X↑          Purpose-dedication(_!_)
;;     _l_ →        _r_ reset       ^_s_wap         _K_  X→          Buffer-dedication(_#_)
;;     ^^^^^^^                                      _M_aximize
;;     ^^^^^^^                                      _d_elete
;;     _x_ M-x      _q_ quit
;;     "
;;   ("h" windmove-left)
;;   ("j" windmove-down)
;;   ("k" windmove-up)
;;   ("l" windmove-right)
;;   ("|" (lambda ()
;;          (interactive)
;;          (split-window-right)
;;          (windmove-right)))
;;   ("-" (lambda ()
;;          (interactive)
;;          (split-window-below)
;;          (windmove-down)))
;;   ("u" (progn
;;          (winner-undo)
;;          (setq this-command 'winner-undo)))
;;   ("r" winner-redo)
;;   ("b" ivy-purpose-switch-buffer-without-purpose)
;;   ("f" counsel-find-file)
;;   ("a" (lambda ()
;;          (interactive)
;;          (ace-window 1)
;;          (add-hook 'ace-window-end-once-hook
;;                    'hydra-window/body)))
;;   ("s" (lambda ()
;;          (interactive)
;;          (ace-swap-window)
;;          (add-hook 'ace-window-end-once-hook
;;                    'hydra-window/body)))
;;   ("H" hydra-move-splitter-left)
;;   ("J" hydra-move-splitter-down)
;;   ("K" hydra-move-splitter-up)
;;   ("L" hydra-move-splitter-right)
;;   ("M" delete-other-windows)
;;   ("d" delete-window)

;;   ("P" purpose-set-window-purpose)
;;   ("B" ivy-purpose-switch-buffer-with-purpose)
;;   ("!" purpose-toggle-window-purpose-dedicated)
;;   ("#" purpose-toggle-window-buffer-dedicated)

;;   ("K" ace-delete-other-windows)
;;   ("S" save-buffer)
;;   ("d" delete-window)
;;   ("D" (lambda ()
;;          (interactive)
;;          (ace-delete-window)
;;          (add-hook 'ace-window-end-once-hook
;;                    'hydra-window/body))
;;    )

;;   ("x" counsel-M-x)
;;   ("q" nil)
;;   )
;; (general-define-key
;;  "<f1>"  'hydra-window/body)

;; (defhydra hydra-windows (:color pink)
;;   "
;; ^
;; ^Windows^           ^Window^            ^Zoom^              ^Eyebrowse Do^            ^Eyebrowse Switch^
;; ^───────^───────────^──────^────────────^────^──────────────^────────────^────────────^────────────────^────────────
;; _q_ quit            _b_ balance         _-_ out             _c_ create                _<_ previous
;; ^^                  _i_ heighten        _+_ in              _k_ kill                  _>_ next
;; ^^                  _j_ narrow          _=_ reset           _r_ rename                _e_ last
;; ^^                  _k_ lower           ^^                  ^^                        _s_ switch
;; ^^                  _l_ widen           ^^                  ^^                        _1_ workspace ➊
;; ^^                  ^^                  ^^                  ^^                        _2_ workspace ➋
;; ^^                  ^^                  ^^                  ^^                        _3_ workspace ➌
;; ^^                  ^^                  ^^                  ^^                        _4_ workspace ➍
;; "
;;   ("q" nil)
;;   ("b" balance-windows)
;;   ("i" enlarge-window)
;;   ("j" shrink-window-horizontally)
;;   ("k" shrink-window)
;;   ("l" enlarge-window-horizontally)
;;   ("-" text-scale-decrease)
;;   ("+" text-scale-increase)
;;   ("=" (text-scale-increase 0))
;;   ("<" eyebrowse-prev-window-config :color red)
;;   (">" eyebrowse-next-window-config :color red)
;;   ("c" eyebrowse-create-window-config)
;;   ("e" eyebrowse-last-window-config)
;;   ("k" eyebrowse-close-window-config :color red)
;;   ("r" eyebrowse-rename-window-config)
;;   ("s" eyebrowse-switch-to-window-config)
;;   ("1" eyebrowse-switch-to-window-config-1)
;;   ("2" eyebrowse-switch-to-window-config-2)
;;   ("3" eyebrowse-switch-to-window-config-3)
;;   ("4" eyebrowse-switch-to-window-config-4))

(provide 'init-window)
;;; init-window.el ends here
