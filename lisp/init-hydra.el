;; init-hydra.el --- Initialize hydra settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; make Emacs bindings that stick around.
;; 参考：https://github.com/abo-abo/hydra/wiki
;; - https://irreal.org/blog/?p=6453
;; - https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org
;; - https://ericjmritz.wordpress.com/2015/10/14/some-personal-hydras-for-gnu-emacs/
;; - https://dfeich.github.io/www/org-mode/emacs/2018/05/10/context-hydra.html
;; - https://www.reddit.com/r/emacs/comments/8of6tx/tip_how_to_be_a_beast_with_hydra/
;; - https://dustinlacewell.github.io/emacs.d/#org1fab346
;; - 这里面有很多 Hydra 配置可以参考 https://github.com/ejmr/DotEmacs/blob/master/init.el
;; - https://github.com/mrbig033/emacs/blob/master/modules/packages/misc/hydra/hydras.org
;; - https://www.reddit.com/r/emacs/comments/c29edh/does_anybody_have_any_more_real_cool_hydras_to
;; 

;;; Code:
(require 'init-funcs)

(use-package hydra
  ;; :preface
  ;; (defvar-local yc/ongoing-hydra-body nil)
  ;; (defun yc/ongoing-hydra ()
  ;;   (interactive)
  ;;   (if yc/ongoing-hydra-body
  ;;       (funcall yc/ongoing-hydra-body)
  ;;     (user-error "yc/ongoing-hydra: yc/ongoing-hydra-body is not set")))
  :general
  ("C-c h <tab>" 'hydra-fold/body
   "C-c h d" 'hydra-dates/body
   "C-c h D" 'hydra-dired/body
   "C-c h f" 'hydra-flycheck/body
   "C-c h j" 'hydra-dump-jump/body
   "C-c h a" 'hydra-avy/body
   "C-c h s" 'hydra-smartparens/body
   "C-c h g" 'hydra-git-timemachine/body
   "C-c h c" 'hydra-multiple-cursors/body
   ;; "C-c g" 'hydra-magit/body
   ;; "C-c h" 'hydra-helm/body
   ;; "C-c o" 'yc/ongoing-hydra
   "C-c h i" 'hydra-imagex-sticky/body
   "C-c h v" 'hydra-pdftools/body
   "C-c h m" 'hydra-macro/body
   "C-c h p" 'hydra-projectile/body
   "C-c h P" 'hydra-system/body
   "C-c h t" 'hydra-toggles/body
   "C-c h w" 'hydra-window/body)
  ;; :config
  ;; (setq hydra-hint-display-type 'my/posframe)
  ;; (defun my/hydra-posframe-show (str)
  ;;   (require 'posframe)
  ;;   (posframe-show
  ;;    " *hydra-posframe*"
  ;;    :string str
  ;;    :point (point)
  ;;    :internal-border-color "gray50"
  ;;    :internal-border-width 2
  ;;    :poshandler #'posframe-poshandler-frame-top-center))
  ;; (defun my/hydra-posframe-hide ()
  ;;   (posframe-hide " *hydra-posframe*"))
  ;; (setq hydra-hint-display-alist
  ;;       (list (list 'my/posframe #'my/hydra-posframe-show #'my/hydra-posframe-hide))
  ;;       hydra--work-around-dedicated nil)
  )

;; 关于 Hydra 高效的按键绑定，参考：
;; - https://github.com/troyp/spacemacs-private/tree/master/docs/hydra-wiki
(use-package pretty-hydra
  :bind ("<f6>" . hydra-toggles/body)
  :init
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                      &key face height v-adjust)
    "Add an icon in the hydra title."
    (let ((face (or face `(:foreground ,(face-background 'highlight))))
          (height (or height 1.0))
          (v-adjust (or v-adjust 0.0)))
      (concat
       (let ((f (intern (format "all-the-icons-%s" icon-type))))
         (when (fboundp f)
           (concat
            (apply f (list icon-name :face face :height height :v-adjust v-adjust))
            " "))))
      (propertize title 'face face)))
  (require 'all-the-icons)
  (defun with-faicon (icon str &optional height v-adjust)
    (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))
  :config
  (defvar jp-toggles--title (with-faicon "toggle-on" "Toggles" 1 -0.05))

  (pretty-hydra-define hydra-toggles
    (:hint nil :color amaranth :quit-key "q" :title jp-toggles--title)
    ("Basic"
     (("n" (if (fboundp 'display-line-numbers-mode)
               (display-line-numbers-mode (if display-line-numbers-mode -1 1))
             (global-display-line-numbers-mode (if global-display-line-numbers-mode -1 1)))
       "line number"
       :toggle (or (bound-and-true-p display-line-numbers-mode) global-display-line-numbers-mode))
      ("a" global-aggressive-indent-mode "aggressive indent" :toggle t)
      ("d" global-hungry-delete-mode "hungry delete" :toggle t)
      ("e" electric-pair-mode "electric pair" :toggle t)
      ("c" flyspell-mode "spell check" :toggle t)
      ("s" prettify-symbols-mode "pretty symbol" :toggle t)
      ("l" global-page-break-lines-mode "page break lines" :toggle t)
      ("b" display-battery-mode "battery" :toggle t)
      ("i" display-time-mode "time" :toggle t)
      ("m" doom-modeline-mode "modern mode-line" :toggle t))
     "Highlight"
     (("h l" global-hl-line-mode "line" :toggle t)
      ("h p" show-paren-mode "paren" :toggle t)
      ("h s" symbol-overlay-mode "symbol" :toggle t)
      ("h r" rainbow-mode "rainbow" :toggle t)
      ("h w" (setq-default show-trailing-whitespace (not show-trailing-whitespace))
       "whitespace" :toggle show-trailing-whitespace)
      ("h d" rainbow-delimiters-mode "delimiter" :toggle t)
      ("h i" highlight-indent-guides-mode "indent" :toggle t)
      ("h t" global-hl-todo-mode "todo" :toggle t))
     "Coding"
     (("p" smartparens-mode "smartparens" :toggle t)
      ("P" smartparens-strict-mode "smartparens strict" :toggle t)
      ("f" flycheck-mode "flycheck" :toggle t)
      ("F" flymake-mode "flymake" :toggle t)
      ("o" origami-mode "folding" :toggle t)
      ("O" hs-minor-mode "hideshow" :toggle t)
      ("u" subword-mode "subword" :toggle t)
      ("W" which-function-mode "which function" :toggle t)
      ("E" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
      ("Q" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit))
      ("v" global-diff-hl-mode "gutter" :toggle t)
      ("V" diff-hl-flydiff-mode "live gutter" :toggle t)
      ("M" diff-hl-margin-mode "margin gutter" :toggle t)
      ("D" diff-hl-dired-mode "dired gutter" :toggle t))
     "Org Mode"
     (("C-l" yc/org-toggle-link-display "link" :toggle t)
      ("C-i" org-toggle-inline-images "image" :toggle t))
     "Dict"
     (("yy" youdao-dictionary-search-at-point+)
      ("yi" youdao-dictionary-search-at-point)
      ("Y" my-youdao-search-at-point)))))

;; (use-package hydra-posframe
;;   :straight (:host github :repo "Ladicle/hydra-posframe")
;;   :after (hydra posframe)
;;   :hook (after-init . hydra-posframe-enable)
;;   :custom-face (hydra-posframe-face ((t (:background "DarkSlateBlue"))))
;;   :custom-face (hydra-posframe-border-face ((t (:background "DarkBlue")))))


;; 这个是参考了 spacemacs 针对 hydra 的扩展，能够方便地绑定 major mode 的按键。
;; 比如，当前的 major mode 是 org-mode，则可以快速唤出相应 org-mode 自定义的 hydra 按键，统一所有 major-mode 的按键。
;; https://github.com/jerrypnz/major-mode-hydra.el
;; 后面的 hydra 配置，要参考 jerrypnz 的 https://github.com/jerrypnz/.emacs.d。
;; Major mode keys managed by a pretty hydra
(use-package major-mode-hydra
  :demand t
  ;; :init
  ;; (progn
  ;;   (autoload 'pretty-hydra-define "pretty-hydra" nil nil 'macro)
  ;;   (autoload 'major-mode-hydra-bind "major-mode-hydra" nil 'macro))
  :config
  (setq major-mode-hydra-separator "═")
  (setq major-mode-hydra-invisible-quit-key "q")
  (setq major-mode-hydra-title-generator
        '(lambda (mode)
           (s-concat "\n"
                     (s-repeat 10 " ")
                     (all-the-icons-icon-for-mode mode :v-adjust 0.05)
                     " "
                     (symbol-name mode)
                     " commands"))))

;; https://github.com/dustinlacewell/hera
(defvar jp-window--title (with-faicon "windows" "Window Management" 1 -0.05))
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

(defhydra hydra-dates (:color red)
  "
  ^
  ^Dates^             ^Insert^            ^Insert with Time^
  ^─────^─────────────^──────^────────────^────────────────^──
  _q_ quit            _d_ short           _D_ short
  ^^                  _i_ iso             _I_ iso
  ^^                  _l_ long            _L_ long
  ^^                  ^^                  ^^
  "
  ("q" nil)
  ("d" me/date-short)
  ("D" me/date-short-with-time)
  ("i" me/date-iso)
  ("I" me/date-iso-with-time)
  ("l" me/date-long)
  ("L" me/date-long-with-time))

(defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                                     :color pink
                                     :hint nil
                                     :post (deactivate-mark))
  "
    ^_k_^       _w_ copy      _o_pen       _N_umber-lines                |\\     -,,,--,,_
  _h_   _l_     _y_ank        _t_ype       _e_xchange-point              /,`.-'`'   ..  \-;;,_
    ^_j_^       _d_ kill      _c_lear      _r_eset-region-mark          |,4-  ) )_   .;.(  `'-'
  ^^^^          _u_ndo        _g_ quit     _C_ua-rectangle-mark-mode   '---''(./..)-'(_\_)
  "
  ("k" rectangle-previous-line)
  ("j" rectangle-next-line)
  ("h" rectangle-backward-char)
  ("l" rectangle-forward-char)
  ("d" kill-rectangle)                   ;; C-x r k
  ("y" yank-rectangle)                   ;; C-x r y
  ("w" copy-rectangle-as-kill)           ;; C-x r M-w
  ("o" open-rectangle)                   ;; C-x r o
  ("t" string-rectangle)                 ;; C-x r t
  ("c" clear-rectangle)                  ;; C-x r c
  ("e" rectangle-exchange-point-and-mark) ;; C-x C-x
  ("N" rectangle-number-lines)            ;; C-x r N
  ("C" cua-rectangle-mark-mode)
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)))
  ("u" undo nil)
  ("g" nil))

(global-set-key (kbd "C-x SPC") 'hydra-rectangle/body)

;; (defhydra hydra-macro (:color teal
;;                               :hint nil)
;;   "
;;   _r_: region  _e_: execute   _c_: counter  _f_: format
;;   _n_: next    _p_: previous  _i_: insert   _q_: query
;;  _(_: start  _)_: stop
;;   "
;;   ("q" nil "quit")
;;   ("Q" kbd-macro-query)
;;   ("(" kmacro-start-macro-or-insert-counter)
;;   (")" kmacro-end-or-call-macro)
;;   ("r" apply-macro-to-region-lines)
;;   ("e" kmacro-end-and-call-macro)
;;   ("n" kmacro-cycle-ring-next)
;;   ("p" kmacro-cycle-ring-previous)
;;   ("i" kmacro-insert-counter)
;;   ("c" kmacro-set-counter)
;;   ("q" kbd-macro-query)
;;   ("f" kmacro-set-format))

;; macro 的按键绑定可以参考 leuven 函数 leuven-kmacro-turn-on-recording
(defun leuven-kmacro-turn-on-recording ()
  "Start recording a keyboard macro and toggle functionality of key binding."
  (interactive)
  (global-set-key (kbd "<S-f3>") #'leuven-kmacro-turn-off-recording)
  (kmacro-start-macro nil))

(defun leuven-kmacro-turn-off-recording ()
  "Stop recording a keyboard macro and toggle functionality of key binding."
  (interactive)
  (global-set-key (kbd "<S-f3>") #'leuven-kmacro-turn-on-recording)
  (kmacro-end-macro nil))

;; Start/stop recording a keyboard macro.
(global-set-key (kbd "<S-f3>") #'leuven-kmacro-turn-on-recording)

;; Execute the most recent keyboard macro.
(global-set-key (kbd "<f3>") #'kmacro-call-macro)

;; hydra for macros in emacs
(defhydra hydra-macro (:hint nil :color pink :pre
                             (when defining-kbd-macro
                               (kmacro-end-macro 1)))
  "
  ^Create-Cycle^   ^Basic^           ^Insert^        ^Save^         ^Edit^
╭─────────────────────────────────────────────────────────────────────────╯
     ^_p_^           [_e_] execute    [_i_] insert    [_b_] name      [_'_] previous
     ^^↑^^           [_d_] delete     [_t_] set       [_K_] key       [_,_] last
 _b_ ←   → _f_     [_o_] edit       [_a_] add       [_x_] register
     ^^↓^^           [_r_] region     [_F_] format    [_B_] defun
     ^_n_^           [_m_] step
    ^^   ^^          [_s_] swap
"
  ("b" kmacro-start-macro :color blue)
  ("f" kmacro-end-or-call-macro-repeat)
  ("p" kmacro-cycle-ring-previous)
  ("n" kmacro-cycle-ring-next)
  ("r" apply-macro-to-region-lines)
  ("d" kmacro-delete-ring-head)
  ("e" kmacro-end-or-call-macro-repeat)
  ("o" kmacro-edit-macro-repeat)
  ("m" kmacro-step-edit-macro)
  ("s" kmacro-swap-ring)
  ("i" kmacro-insert-counter)
  ("t" kmacro-set-counter)
  ("a" kmacro-add-counter)
  ("F" kmacro-set-format)
  ("b" kmacro-name-last-macro)
  ("K" kmacro-bind-to-key)
  ("B" insert-kbd-macro)
  ("x" kmacro-to-register)
  ("'" kmacro-edit-macro)
  ("," edit-kbd-macro)
  ("q" nil "Quit" :color blue))

(provide 'init-hydra)
