;; init-hydra.el --- Initialize hydra settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; make Emacs bindings that stick around.
;; 参考：
;; - https://github.com/abo-abo/hydra/wiki
;; - https://irreal.org/blog/?p=6453
;; - https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org
;; - https://ericjmritz.wordpress.com/2015/10/14/some-personal-hydras-for-gnu-emacs/
;; - https://dfeich.github.io/www/org-mode/emacs/2018/05/10/context-hydra.html
;; - https://www.reddit.com/r/emacs/comments/8of6tx/tip_how_to_be_a_beast_with_hydra/
;; - https://dustinlacewell.github.io/emacs.d/#org1fab346
;; 这里面有很多 Hydra 配置可以参考
;; - https://github.com/ejmr/DotEmacs/blob/master/init.el
;; - https://github.com/mrbig033/emacs/blob/master/modules/packages/misc/hydra/hydras.org
;; - https://www.reddit.com/r/emacs/comments/c29edh/does_anybody_have_any_more_real_cool_hydras_to
;;
;; https://github.com/abo-abo/hydra/wiki/Hydra-Colors#colorful-hydras
;; https://github.com/abo-abo/hydra/wiki/internals#exit
;; |----------+-----------------------------+-----------+-----------------------+-----------|
;; | Body     | Non-color                   | Head      | Executing             | After     |
;; | Color    | Alternative                 | Inherited | NON-HEADS             | executing |
;; |          |                             | Color     |                       | HEADS     |
;; |----------+-----------------------------+-----------+-----------------------+-----------|
;; | red      | :foreign-keys nil (default) | red       | Allow and Quit        |           |
;; |          | :exit nil (default)         |           |                       | Continue  |
;; |----------+-----------------------------+-----------+-----------------------+-----------|
;; | blue     | :foreign-keys nil (default) | blue      | Allow and Quit        |           |
;; |          | :exit t                     |           |                       | Quit      |
;; |----------+-----------------------------+-----------+-----------------------+-----------|
;; | amaranth | :foreign-keys warn          | red       | Disallow and Continue |           |
;; |          | :exit nil (default)         |           |                       | Continue  |
;; |----------+-----------------------------+-----------+-----------------------+-----------|
;; | teal     | :foreign-keys warn          | blue      | Disallow and Continue |           |
;; |          | :exit t                     |           |                       | Quit      |
;; |----------+-----------------------------+-----------+-----------------------+-----------|
;; | pink     | :foreign-keys run           | red       | Allow and Continue    |           |
;; |          | :exit nil (default)         |           |                       | Continue  |
;; |----------+-----------------------------+-----------+-----------------------+-----------|
;;

;;; Code:
(require 'init-funcs)

(use-package hydra
  ;; novoid 的很多 hydra 配置值得参考：
  ;; - /Users/yangc/src/emacs.d/novoid-dot-emacs/config.org
  ;; - /Users/yangc/src/emacs.d/caiohcs-emacs/settings.el
  :bind
  (("C-z <tab>" . hydra-fold/body)
   ("C-z D"     . hydra-dates/body)
   ("C-z f"     . hydra-flycheck/body)
   ("C-z j"     . hydra-dumb-jump/body)
   ("C-z a"     . hydra-avy/body)
   ("C-z s"     . hydra-selected/body)
   ("C-z g"     . hydra-git-timemachine/body)
   ("C-z m"     . hydra-macro/body)
   ("C-z p"     . hydra-projectile/body)
   ("C-z P"     . hydra-system/body)
   ("C-z t"     . hydra-toggles/body)
   ("C-z w"     . hydra-window/body))
  :config
  ;; (which-key-add-key-based-replacements "C-c h"   "hydra")
  (defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                                       :color pink
                                       :hint nil
                                       :post (deactivate-mark))
    "
                     ^_k_^       _w_ copy      _o_pen       _N_umber-lines                |\\     -,,,--,,_
                   _h_   _l_     _y_ank        _t_ype       _e_xchange-point              /,`.-'`'   ..  \-;;,_
                     ^_j_^       _d_ kill      _c_lear      _r_eset-region-mark          |,4-  ) )_   .;.(  `'-'
  ^^^^              _u_ndo        _C-g_ quit     _C_ua-rectangle-mark-mode              '---''(./..)-'(_\_)
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
    ("C-g" nil))
  (global-set-key (kbd "C-x SPC") 'hydra-rectangle/body)

  ;; hydra for macros in emacs
  (pretty-hydra-define hydra-macro
    (:hint nil :color pink :quit-key "C-g" :pre (when defining-kbd-macro
                                                  (kmacro-end-macro 1)))
    ("Create-Cycle"
     (("s" kmacro-start-macro :color blue)   ;; C-x (
      ("(" kmacro-start-macro-or-insert-counter :color blue)
      
      (")" kmacro-end-and-call-macro)        ;; C-x )
      ("p" kmacro-cycle-ring-previous)
      ("n" kmacro-cycle-ring-next))
     "Basic"
     (("r" apply-macro-to-region-lines)
      ("d" kmacro-delete-ring-head)
      ("e" kmacro-end-or-call-macro-repeat)
      ("o" kmacro-edit-macro-repeat)
      ("m" kmacro-step-edit-macro)
      ("S" kmacro-swap-ring))
     "Insert"
     (("i" kmacro-insert-counter)
      ("t" kmacro-set-counter)
      ("a" kmacro-add-counter)
      ("F" kmacro-set-format))
     "Save"
     (("b" kmacro-name-last-macro)
      ("K" kmacro-bind-to-key)
      ("B" insert-kbd-macro)
      ("x" kmacro-to-register))
     "Edit"
     (("'" kmacro-edit-macro)
      ("," edit-kbd-macro))))
  (defhydra hydra-dates (:color red)
    "
                     ^
                     ^Dates^             ^Insert^            ^Insert with Time^
                     ^─────^─────────────^──────^────────────^────────────────^──
                     _C-g_ quit            _d_ short           _D_ short
                     ^^                  _i_ iso             _I_ iso
                     ^^                  _l_ long            _L_ long
                     ^^                  ^^                  ^^
                     "
    ("C-g" nil)
    ("d" me/date-short)
    ("D" me/date-short-with-time)
    ("i" me/date-iso)
    ("I" me/date-iso-with-time)
    ("l" me/date-long)
    ("L" me/date-long-with-time)))

;; 关于 Hydra 高效的按键绑定，参考：
;; - https://github.com/troyp/spacemacs-private/tree/master/docs/hydra-wiki
(use-package pretty-hydra
  ;; :bind
  ;; ([f6] . hydra-toggles/body)
  :init
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name &key face height v-adjust)
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
  (require 'nerd-icons)
  (defun with-faicon (icon str &optional height v-adjust)
    (s-concat (nerd-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))
  :config
  (defvar jp-toggles--title (with-faicon "nf-fa-toggle_on" "Toggles" 1 -0.05))
  (pretty-hydra-define hydra-toggles
    (:hint nil :color amaranth :quit-key "C-g" :title jp-toggles--title)
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
      ("S" prettify-symbols-mode "pretty symbol" :toggle t)
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
      ("h i" indent-bars-mode "indent" :toggle t)
      ("h t" global-hl-todo-mode "todo" :toggle t))
     "Coding"
     (("p" smartparens-mode "smartparens" :toggle t)
      ("P" smartparens-strict-mode "smartparens strict" :toggle t)
      ("f" flycheck-mode "flycheck" :toggle t)
      ("s" sideline-mode "sideline" :toggle t)
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
     "Translate"
     (("t y" youdao-dictionary-search-at-point+)
      ("t i" youdao-dictionary-search-at-point)
      ("t Y" yc/youdao-search-at-point)
      ("t f" fanyi-dwim2)
      ("t g" gt-do-translate)))))

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
  (setq major-mode-hydra-invisible-quit-key "C-g")
  (setq major-mode-hydra-title-generator
        '(lambda (mode)
           (s-concat "\n"
                     (s-repeat 10 " ")
                     (all-the-icons-icon-for-mode mode :v-adjust 0.05)
                     " "
                     (symbol-name mode)
                     " commands"))))

;;================================================================================
;; (use-package hydra-posframe
;;   :straight (:host github :repo "Ladicle/hydra-posframe")
;;   :after (hydra posframe)
;;   :hook (after-init . hydra-posframe-enable)
;;   :custom-face (hydra-posframe-face ((t (:background "DarkSlateBlue"))))
;;   :custom-face (hydra-posframe-border-face ((t (:background "DarkBlue"))))
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
;; )
;;================================================================================

(provide 'init-hydra)
;;; init-hydra.el ends here
