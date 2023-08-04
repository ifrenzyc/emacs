;; init-edit.el --- Initialize edit configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; edit enhance

;;; Code:

;; 当文件内容有修改时，可以通过 autorevert 重新加载这个文件
(use-package autorevert
   :hook ((after-init . global-auto-revert-mode)
          (dired-mode . auto-revert-mode)))

;; 关闭 emacs 后，重新打开文件时跳转到上一次打开该文件时的位置（所在行）。
;; Restore cursor to file position in previous editing session.
;; 参考文档：https://www.emacswiki.org/emacs/SavePlace#toc1
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package savehist
  :disabled t
  :ensure nil
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              history-length 1000
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 300))

;; 文本替换，使用 =query-replace= 或者 =M-%= 命令。
;; - https://github.com/syohex/emacs-anzu
;; anzu 依赖这个 mdi
;; (use-package mdi
;;   :load-path "localelpa/mdi/")

;; 另外一个与 anzu 替代： https://github.com/benma/visual-regexp.el 
(use-package anzu
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :hook (after-init . global-anzu-mode)
  :config
  (set-face-attribute 'anzu-mode-line nil
                      :foreground "yellow" :weight 'bold)
  (setq-default anzu-cons-mode-line-p nil
                anzu-replace-to-string-separator " => "
                ;; anzu-replace-to-string-separator (mdi "arrow-right" t)
                ))

;; *About:* 快速编辑选中的区域
;; Edit multiple regions in the same way simultaneously
;; 参考：https://github.com/andreyorst/dotfiles/tree/master/.config/emacs
(use-package iedit
  :disabled t
  :bind (("M-n" . aorst/iedit-current-or-expand))
  :custom
  (iedit-toggle-key-default nil)
  :init
  (defun aorst/iedit-to-mc-hydrant ()
    "Calls `iedit-to-mc-mode' and opens hydra for multiple cursors."
    (interactive)
    (iedit-switch-to-mc-mode)
    (hydra-iedit/body))
  (defun aorst/iedit-current-or-expand (&optional arg)
    "Select only currnent occurrence with `iedit-mode'.  Expand to
  next occurrence if `iedit-mode' is already active."
    (interactive "P")
    (if (bound-and-true-p iedit-mode)
        (if (symbolp arg)
            (iedit-expand-down-to-occurrence)
          (iedit-expand-up-to-occurrence))
      (iedit-mode 1)))
  (defun aorst/iedit-hydrant ()
    "toggle iedit mode for item under point, and open `hydrant/iedit'."
    (interactive)
    (ignore-errors
      (iedit-mode 1)
      (hydra-iedit/body)))
  (defhydra hydra-iedit (:hint nil :color pink)
    "
 ^Select^                  ^Discard^                   ^Edit^               ^Navigate^
─^──────^──────────────────^───────^───────────────────^────^───────────────^────────^─────────────
 _n_: next occurrence      _M-SPC_:  toggle selection  _u_: uppercase       _(_: previous selection
 _p_: previous occurrence  _q_ or _g_: exit hydrant      _d_: downcase        _)_: next selection
 ^ ^                       _G_:      exit iedit-mode   _#_: insert numbers
 ^ ^                       _m_:      switch to mc"
    ("n" iedit-expand-down-to-occurrence)
    ("m" aorst/iedit-to-mc-hydrant :exit t)
    ("p" iedit-expand-up-to-occurrence)
    ("u" iedit-upcase-occurrences)
    ("d" iedit-downcase-occurrences)
    ("#" iedit-number-occurrences)
    ("(" iedit-prev-occurrence)
    (")" iedit-next-occurrence)
    ("M-SPC" iedit-toggle-selection)
    ("q" ignore :exit t)
    ("g" ignore :exit t)
    ("G" #'(lambda () (interactive) (iedit-mode -1)) :exit t)))

;; 参考：http://www.cauchy.me/2015/08/20/emacs-multi-cursors/
(use-package multiple-cursors
  :disabled t
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("S-<mouse-1>" . mc/add-cursor-on-click)
   ;; ("C-c m" . hydra-multiple-cursors/body)
   )
  :requires hydra
  :config
  (defhydra hydra-multiple-cursors (:hint nil :color pink)
    "
 ^Select^                 ^Discard^                     ^Edit^               ^Navigate^
─^──────^─────────────────^───────^─────────────────────^────^───────────────^────────^─────────
 _M-s_: split lines       _M-SPC_:  discard current      _&_: align           _(_: cycle backward
 _s_:   select regexp     _b_:      discard blank lines  _#_: insert numbers  _)_: cycle forward
 _n_:   select next       _d_:      remove duplicated    ^ ^                  ^ ^
 _p_:   select previous   _q_ or _g_: exit hydrant       ^ ^                  ^ ^
 _C_:   select next line  _G_:      exit mc mode"
    ("M-s" mc/edit-ends-of-lines)
    ("s" mc/mark-all-in-region-regexp)
    ("n" mc/mark-next-like-this-word)
    ("p" mc/mark-previous-like-this-word)
    ("&" mc/vertical-align-with-space)
    ("(" mc/cycle-backward)
    (")" mc/cycle-forward)
    ("M-SPC" mc/remove-current-cursor)
    ("b" mc/remove-cursors-on-blank-lines)
    ("d" mc/remove-duplicated-cursors)
    ("C" mc/mark-next-lines)
    ("#" mc/insert-numbers)
    ("q" mc/remove-duplicated-cursors :exit t)
    ("g" mc/remove-duplicated-cursors :exit t)
    ("G" mc/keyboard-quit :exit t))

  ;; (defhydra hydra-multiple-cursors (:hint nil)
  ;;   "
  ;;    Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
  ;;   ------------------------------------------------------------------
  ;;    [_p_]   Next     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
  ;;    [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
  ;;    [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search
  ;;    [Click] Cursor at point       [_q_] Quit"
  ;;   ("l" mc/edit-lines :exit t)
  ;;   ("a" mc/mark-all-like-this :exit t)
  ;;   ("n" mc/mark-next-like-this)
  ;;   ("N" mc/skip-to-next-like-this)
  ;;   ("M-n" mc/unmark-next-like-this)
  ;;   ("p" mc/mark-previous-like-this)
  ;;   ("P" mc/skip-to-previous-like-this)
  ;;   ("M-p" mc/unmark-previous-like-this)
  ;;   ("s" mc/mark-all-in-region-regexp :exit t)
  ;;   ("0" mc/insert-numbers :exit t)
  ;;   ("A" mc/insert-letters :exit t)
  ;;   ("<mouse-1>" mc/add-cursor-on-click)
  ;;   ;; Help with click recognition in this hydra
  ;;   ("<down-mouse-1>" ignore)
  ;;   ("<drag-mouse-1>" ignore)
  ;;   ("q" nil))
  )

(use-package mc-extras
  :disabled t
  :after multiple-cursors)

;; Emacs extension to increase selected region by semantic units.
;; - https://github.com/magnars/expand-region.el
(use-package expand-region
  :requires hydra
  :init
  (pending-delete-mode t)
  :general
  ("C-=" 'er/expand-region)
  ;; ("C-c =" 'bk/expand-region/body)
  ;; (evil-visual-state-map
  ;;  "v" 'er/expand-region)
  )

;; 针对选中的区域自定义一些按键
;; - https://github.com/Kungsgeten/selected.el
(use-package selected
  :commands (selected-minor-mode)
  :custom
  (selected-minor-mode-override t)
  :hook
  (after-init . selected-global-mode)
  :config
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)

  (defvar-local me/pretty-print-function nil)
  (defun me/pretty-print (beg end)
    (interactive "r")
    (if me/pretty-print-function
        (progn (funcall me/pretty-print-function beg end)
               (setq deactivate-mark t))
      (user-error "me/pretty-print: me/pretty-print-function is not set")))

  ;; https://rejeep.github.io/emacs/elisp/2010/03/11/duplicate-current-line-or-region-in-emacs.html
  (defun duplicate-current-line-or-region (arg)
    "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
    (interactive "p")
    (let (beg end (origin (point)))
      (if (and mark-active (> (point) (mark)))
          (exchange-point-and-mark))
      (setq beg (line-beginning-position))
      (if mark-active
          (exchange-point-and-mark))
      (setq end (line-end-position))
      (let ((region (buffer-substring-no-properties beg end)))
        (dotimes (i arg)
          (goto-char end)
          (newline)
          (insert region)
          (setq end (point)))
        (goto-char (+ origin (* (length region) arg) arg)))))

  (defun align-code (beg end &optional arg)
    (interactive "rP")
    (if (null arg)
        (align beg end)
      (let ((end-mark (copy-marker end)))
        (indent-region beg end-mark nil)
        (align beg end-mark))))

  (defun yc/show-selected-mode-keymap ()
    (interactive)
    (which-key-show-keymap 'selected-keymap))

  ;; 这是个不错的参考：/Users/yangc/src/emacs.d/otijhuis-emacs.d/config/hydra-settings.el hydra-mark/body
  (pretty-hydra-define hydra-selected
    (:hint nil :foreign-keys warn :quit-key "C-g" :title "Active Region" :separator "═")
    ("Selected"
     (("M-l" yc/show-selected-mode-keymap)
      ("q" selected-off)
      ("s" sort-lines)
      ("f" fill-region)
      ("r" reverse-region)
      ("w" count-words-region)
      ("u" upcase-region)
      ("l" downcase-region)
      ("m" apply-macro-to-region-lines)
      ("[" align-code)
      ("a" align-regexp)
      ("C-d" duplicate-current-line-or-region)
      ("C-c c" capitalize-region))
     "Expand/Contract"
     (("e" er/expand-region)
      ("C-x C-x" exchange-point-and-mark)
      ("+" er/expand-region)
      ("R" er/contract-region)
      ("-" er/contract-region))
     "Mark"
     (("p" er/mark-paragraph)
      ("(" er/mark-inside-pairs)
      ;; ("q" er/mark-inside-quotes)
      ("'" er/mark-inside-quotes)
      ("Q" er/mark-outside-quotes)
      ("\"" er/mark-outside-quotes)
      ("G" #'(lambda () (interactive) (deactivate-mark)) :exit t))
     "Symbol Overlay"
     (("M-i" symbol-overlay-put)
      ("M-n" symbol-overlay-jump-next)
      ("M-p" symbol-overlay-jump-prev)
      ("M-N" symbol-overlay-switch-forward)
      ("M-P" symbol-overlay-switch-backward)
      ("M-C" symbol-overlay-remove-all)
      ("s-." symbol-overlay-transient)
      ("M-<f3>" symbol-overlay-remove-all))
     "Embark"
     (("s-o" embark-act))))
  )

(use-package barrinalo
  :disabled t
  :load-path "localelpa/barrinalo"
  :bind
  ("M-p" . barrinalo-swap-up)
  ("M-n" . barrinalo-swap-down)
  ("M-P" . barrinalo-duplicate-backward)
  ("M-N" . barrinalo-duplicate-forward))

;; 选中文本后，直接输入就可以，省去了删除操作。这在其他文本编辑器里都是标配，建议打开。
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; Hungry deletion
(use-package hungry-delete
  :hook
  (after-init . global-hungry-delete-mode)
  (minibuffer-setup . (lambda () (hungry-delete-mode -1)))
  :config
  (setq-default hungry-delete-chars-to-skip " \t\f\v"))

(use-package subword
  :ensure nil
  :delight subword-mode)

(use-package beginend
  :diminish (beginend-mode beginend-global-mode)
  :hook (after-init . beginend-global-mode))

;; Move to the beginning/end of line or code
(use-package mwim
  :demand t
  :bind (;; ([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
         ;; ([remap move-end-of-line] . mwim-end-of-code-or-line)
         ("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line))
  ;; :config  
  ;; `C-a' first takes you to the first non-whitespace char as
  ;; `back-to-indentation' on a line, and if pressed again takes you to
  ;; the actual beginning of the line.
  ;; (defun smarter-move-beginning-of-line (arg)
  ;;   "Move depending on ARG to beginning of visible line or not.
  ;; From https://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/."
  ;;   (interactive "^p")
  ;;   (setq arg (or arg 1))
  ;;   (when (/= arg 1)
  ;;     (let ((line-move-visual nil))
  ;;       (forward-line (1- arg))))
  ;;   (let ((orig-point (point)))
  ;;     (back-to-indentation)
  ;;     (when (= orig-point (point))
  ;;       (move-beginning-of-line 1))))
  ;; (global-set-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)
  )

;; View Large Files
;; - https://github.com/m00natic/vlfi
;; - https://writequit.org/articles/working-with-logs-in-emacs.html
(use-package vlf
  :config
  (require 'vlf-setup))

;; An all-in-one comment command to rule them all
(use-package comment-dwim-2
  :bind ([remap comment-dwim] . comment-dwim-2))

;; 针对中文用户，在中英文混合的文档里面，在中英文中间显示空格字符。
;; - https://github.com/coldnew/pangu-spacing
;; - http://coldnew.github.io/blog/2013/05-20_5cbb7/
(use-package pangu-spacing
  ;; :hook
  ;; ;; 针对 org-mode 和 markdown-mode，插入真正的空格字符
  ;; (org-mode . (lambda ()
  ;;               (pangu-spacing-mode 1)
  ;;               (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)))
  ;; (markdown-mode . (lambda ()
  ;;                    (pangu-spacing-mode 1)
  ;;                    (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)))
  :config
  (global-pangu-spacing-mode t))

;; 高亮显示匹配的分隔符，自动输入删除
;; - https://github.com/Fuco1/smartparens
;; - https://ebzzry.io/en/emacs-pairs/ (paredit: http://emacsrocks.com/e14.html)
;; Highlight matching and auto insert parenthesis.
;; 这个要看看，结合 evil 要怎么用，或者有其他的 mode 可以替代，比如 evil-xxx？
;; (use-package smartparens
;;   ;; :after (evil general)
;;   :after (general)
;;   :init
;;   ;; (require 'smartparens-config)
;;   ;; (require 'smartparens-org)
;;   (setq sp-highlight-pair-overlay nil)
;;   :config
;;   (smartparens-global-mode t)
;;   ;; Toggle Show-Smartparens mode in all buffers.
;;   (show-smartparens-global-mode 1)
;;   ;; (smartparens-strict-mode t)
;;   ;; (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
;;   ;; (add-hook 'org-mode-hook 'turn-on-smartparens-strict-mode)
;;   (setq sp-show-pair-from-inside t)

;;   (sp-with-modes '(clojure-mode cider-repl-mode)
;;                  (sp-local-pair "#{" "}")
;;                  (sp-local-pair "`" nil :actions nil)
;;                  (sp-local-pair "@(" ")")
;;                  (sp-local-pair "#(" ")"))

;;   ;; include new wrap of pairs
;;   (sp-pair "(" ")" :wrap "M-(")
;;   (sp-pair "[" "]" :wrap "M-[")

;;   (sp-local-pair 'markdown-mode "`" nil :actions nil)
;;   (sp-local-pair 'gfm-mode "`" nil :actions nil)
;;   (sp-local-pair 'web-mode "{" "}" :actions nil)
;;   ;; (-each sp--lisp-modes 'enable-lisp-hooks)

;;   (defun indent-between-pair (&rest _ignored)
;;     (newline)
;;     (indent-according-to-mode)
;;     (forward-line -1)
;;     (indent-according-to-mode))

;;   (sp-local-pair 'prog-mode "{" nil :post-handlers '((indent-between-pair "RET")))
;;   (sp-local-pair 'prog-mode "[" nil :post-handlers '((indent-between-pair "RET")))
;;   (sp-local-pair 'prog-mode "(" nil :post-handlers '((indent-between-pair "RET")))

;;   (custom-set-faces
;;    '(sp-show-pair-match-face ((t (:weight regular :foreground "SpringGreen3" :underline t)))))
;;   :general
;;   (smartparens-mode-map
;;    ;; custom keybindings for smartparens mode
;;    "M-("       'sp-forward-barf-sexp
;;    "M-)"       'sp-forward-slurp-sexp
;;    "s-S"       'sp-split-sexp)
;;   (smartparens-strict-mode-map
;;    "M-d" 'kill-sexp
;;    "M-D" 'sp-kill-sexp))

;; ;; - https://github.com/expez/evil-smartparens
;; (use-package evil-smartparens
;;   :after (evil smartparens)
;;   :config
;;   (defvar show-paren-delay 0)
;;   :hook
;;   (smartparens-enabled . evil-smartparens-mode))

;; ;; https://github.com/abo-abo/hydra/wiki/Smartparens
;; (defhydra hydra-smartparens (:hint nil)
;;   "
;;    Moving^^^^                       Slurp & Barf^^   Wrapping^^            Sexp juggling^^^^               Destructive
;;   ------------------------------------------------------------------------------------------------------------------------
;;    [_a_] beginning  [_n_] down      [_h_] bw slurp   [_R_]   rewrap        [_S_] split   [_t_] transpose   [_c_] change inner  [_w_] copy
;;    [_e_] end        [_N_] bw down   [_H_] bw barf    [_u_]   unwrap        [_s_] splice  [_A_] absorb      [_C_] change outer
;;    [_f_] forward    [_p_] up        [_l_] slurp      [_U_]   bw unwrap     [_r_] raise   [_E_] emit        [_k_] kill          [_g_] quit
;;    [_b_] backward   [_P_] bw up     [_L_] barf       [_(__{__[_] wrap (){}[]   [_j_] join    [_o_] convolute   [_K_] bw kill       [_q_] quit"
;;   ;; Moving
;;   ("a" sp-beginning-of-sexp)
;;   ("e" sp-end-of-sexp)
;;   ("f" sp-forward-sexp)
;;   ("b" sp-backward-sexp)
;;   ("n" sp-down-sexp)
;;   ("N" sp-backward-down-sexp)
;;   ("p" sp-up-sexp)
;;   ("P" sp-backward-up-sexp)

;;   ;; Slurping & barfing
;;   ("h" sp-backward-slurp-sexp)
;;   ("H" sp-backward-barf-sexp)
;;   ("l" sp-forward-slurp-sexp)
;;   ("L" sp-forward-barf-sexp)

;;   ;; Wrapping
;;   ("R" sp-rewrap-sexp)
;;   ("u" sp-unwrap-sexp)
;;   ("U" sp-backward-unwrap-sexp)
;;   ("(" sp-wrap-round)
;;   ("{" sp-wrap-curly)
;;   ("[" sp-wrap-square)

;;   ;; Sexp juggling
;;   ("S" sp-split-sexp)
;;   ("s" sp-splice-sexp)
;;   ("r" sp-raise-sexp)
;;   ("j" sp-join-sexp)
;;   ("t" sp-transpose-sexp)
;;   ("A" sp-absorb-sexp)
;;   ("E" sp-emit-sexp)
;;   ("o" sp-convolute-sexp)

;;   ;; Destructive editing
;;   ("c" sp-change-inner :exit t)
;;   ("C" sp-change-enclosing :exit t)
;;   ("k" sp-kill-sexp)
;;   ("K" sp-backward-kill-sexp)
;;   ("w" sp-copy-sexp)

;;   ("q" nil)
;;   ("g" nil))

;; - https://emacs-china.org/t/awesome-pair-el/7558/39
;; (use-package awesome-pair
;;   :straight (:host github :repo "manateelazycat/awesome-pair")
;;   :config
;;   (dolist (hook (list
;;                  'c-mode-common-hook
;;                  'c-mode-hook
;;                  'c++-mode-hook
;;                  'java-mode-hook
;;                  'haskell-mode-hook
;;                  'emacs-lisp-mode-hook
;;                  'lisp-interaction-mode-hook
;;                  'lisp-mode-hook
;;                  'maxima-mode-hook
;;                  'ielm-mode-hook
;;                  'sh-mode-hook
;;                  'makefile-gmake-mode-hook
;;                  'php-mode-hook
;;                  'python-mode-hook
;;                  'js-mode-hook
;;                  'go-mode-hook
;;                  'qml-mode-hook
;;                  'jade-mode-hook
;;                  'css-mode-hook
;;                  'ruby-mode-hook
;;                  'coffee-mode-hook
;;                  'rust-mode-hook
;;                  'qmake-mode-hook
;;                  'lua-mode-hook
;;                  'swift-mode-hook
;;                  'minibuffer-inactive-mode-hook
;;                  ))
;;     (add-hook hook '(lambda () (awesome-pair-mode 1))))

;;   (define-key awesome-pair-mode-map (kbd "(") 'awesome-pair-open-round)
;;   (define-key awesome-pair-mode-map (kbd "[") 'awesome-pair-open-bracket)
;;   (define-key awesome-pair-mode-map (kbd "{") 'awesome-pair-open-curly)
;;   (define-key awesome-pair-mode-map (kbd ")") 'awesome-pair-close-round)
;;   (define-key awesome-pair-mode-map (kbd "]") 'awesome-pair-close-bracket)
;;   (define-key awesome-pair-mode-map (kbd "}") 'awesome-pair-close-curly)
;;   (define-key awesome-pair-mode-map (kbd "=") 'awesome-pair-equal)

;;   (define-key awesome-pair-mode-map (kbd "%") 'awesome-pair-match-paren)
;;   (define-key awesome-pair-mode-map (kbd "\"") 'awesome-pair-double-quote)

;;   (define-key awesome-pair-mode-map (kbd "SPC") 'awesome-pair-space)

;;   (define-key awesome-pair-mode-map (kbd "M-o") 'awesome-pair-backward-delete)
;;   (define-key awesome-pair-mode-map (kbd "C-d") 'awesome-pair-forward-delete)
;;   (define-key awesome-pair-mode-map (kbd "C-k") 'awesome-pair-kill)

;;   (define-key awesome-pair-mode-map (kbd "M-\"") 'awesome-pair-wrap-double-quote)
;;   (define-key awesome-pair-mode-map (kbd "M-[") 'awesome-pair-wrap-bracket)
;;   (define-key awesome-pair-mode-map (kbd "M-{") 'awesome-pair-wrap-curly)
;;   (define-key awesome-pair-mode-map (kbd "M-(") 'awesome-pair-wrap-round)
;;   (define-key awesome-pair-mode-map (kbd "M-)") 'awesome-pair-unwrap)

;;   (define-key awesome-pair-mode-map (kbd "M-p") 'awesome-pair-jump-right)
;;   (define-key awesome-pair-mode-map (kbd "M-n") 'awesome-pair-jump-left)
;;   (define-key awesome-pair-mode-map (kbd "M-:") 'awesome-pair-jump-out-pair-and-newline))

;; - https://github.com/nivekuil/corral
(use-package corral
  :disabled t
  :config
  (defhydra hydra-corral (:columns 4)
    "Corral"
    ("(" corral-parentheses-backward "Back")
    (")" corral-parentheses-forward "Forward")
    ("[" corral-brackets-backward "Back")
    ("]" corral-brackets-forward "Forward")
    ("{" corral-braces-backward "Back")
    ("}" corral-braces-forward "Forward")
    ("." hydra-repeat "Repeat")))

(use-package cycle-quotes
  :disabled t
  :commands (cycle-quotes))

(use-package hippie-expand
  :ensure nil
  :init
  (setq hippie-expand-try-functions-list
	    '(
	      ;; Try to expand yasnippet snippets based on prefix
	      yas-hippie-try-expand
	      ;; Try to expand word "dynamically", searching the current buffer.
	      try-expand-dabbrev
	      ;; Try to expand word "dynamically", searching all other buffers.
	      try-expand-dabbrev-all-buffers
	      ;; Try to expand word "dynamically", searching the kill ring.
	      try-expand-dabbrev-from-kill
	      ;; Try to complete text as a file name, as many characters as unique.
	      try-complete-file-name-partially
	      ;; Try to complete text as a file name.
	      try-complete-file-name
	      ;; Try to expand word before point according to all abbrev tables.
	      try-expand-all-abbrevs
	      ;; Try to complete the current line to an entire line in the buffer.
	      try-expand-list
	      ;; Try to complete the current line to an entire line in the buffer.
	      try-expand-line
	      ;; Try to complete as an Emacs Lisp symbol, as many characters as
	      ;; unique.
	      try-complete-lisp-symbol-partially
	      ;; Try to complete word as an Emacs Lisp symbol.
	      try-complete-lisp-symbol
	      ))
  :general
  ("M-/" 'hippie-expand)
  :config
  ;; override dabbrev-expand’s keybinding to use hippie-expand instead
  (define-key (current-global-map) [remap dabbrev-expand] 'hippie-expand))

(use-package fancy-dabbrev
  :commands (fancy-dabbrev-mode)
  :custom
  (fancy-dabbrev-preview-delay 0.1)
  (fancy-dabbrev-preview-context 'before-non-word)

  (fancy-dabbrev-expansion-on-preview-only t)
  (fancy-dabbrev-indent-command 'tab-to-tab-stop)

  ;; Only while in insert mode.
  ;; (with-eval-after-load 'evil
  ;;   (add-hook 'evil-insert-state-entry-hook (lambda () (fancy-dabbrev-mode 1)))
  ;;   (add-hook 'evil-insert-state-exit-hook (lambda () (fancy-dabbrev-mode 0))))
  )

;; - Zap To Char Usage :: https://www.emacswiki.org/emacs/ZapToCharUsage
(use-package zzz-to-char
  :bind
  ([remap zap-to-char] . zzz-to-char))

(provide 'init-edit)
;;; init-edit.el ends here
