;; init-highlight.el --- Initialize highlighting things configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:
(require 'init-const)

;; 实现类似在 vs 编辑器中，按 =<F3>= 快速搜索光标当前所在的词。
;; 当光标移到某个词上面的时候自动高亮显示当前的词，当光标处在某个词上面时，高亮其他相同的词。 /效果如下:/

;; [[file:./screenshots/highlight-symbol-at-point.gif]]

;; - GitHub: https://github.com/nschum/highlight-symbol.el
;; - https://emacs-china.org/t/package-symbol-overlay-symbol/7706 (https://github.com/wolray/symbol-overlay/)
;; - https://github.com/gennad/auto-highlight-symbol
;; | Key      | Function                  | Desc                 |
;; |----------+---------------------------+----------------------|
;; | =M-<F3>= | highlight-symbol-at-point | 高亮光标当前所在的词   |
;; | =<F3>=   | highlight-symbol-next     | 查找下一个匹配的词     |
;; | =S-<F3>= | highlight-symbol-prev     | 查找上一个匹配的词     |

;; TODO 参考这个优化按键及显示方式
;; https://github.com/kaz-yos/emacs/blob/master/init.d/500_highlight-search-replace-related.el

;; 打开括号匹配显示模式
;; Highlight matching parens
(use-package paren
  :ensure nil
  :hook
  (after-init . show-paren-mode)
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  ;; 括号匹配时可以高亮显示另外一边的括号，但光标不会烦人的跳到另一个括号处。
  (show-paren-style 'parenthesis)
  :config
  (with-no-warnings
    ;; Display matching line for off-screen paren.
    (defun display-line-overlay (pos str &optional face)
      "Display line at POS as STR with FACE.

FACE defaults to inheriting from default and highlight."
      (let ((ol (save-excursion
                  (goto-char pos)
                  (make-overlay (line-beginning-position)
                                (line-end-position)))))
        (overlay-put ol 'display str)
        (overlay-put ol 'face
                     (or face '(:inherit highlight)))
        ol))

    (defvar-local show-paren--off-screen-overlay nil)
    (defun show-paren-off-screen (&rest _args)
      "Display matching line for off-screen paren."
      (when (overlayp show-paren--off-screen-overlay)
        (delete-overlay show-paren--off-screen-overlay))
      ;; Check if it's appropriate to show match info,
      (when (and (overlay-buffer show-paren--overlay)
                 (not (or cursor-in-echo-area
                          executing-kbd-macro
                          noninteractive
                          (minibufferp)
                          this-command))
                 (and (not (bobp))
                      (memq (char-syntax (char-before)) '(?\) ?\$)))
                 (= 1 (logand 1 (- (point)
                                   (save-excursion
                                     (forward-char -1)
                                     (skip-syntax-backward "/\\")
                                     (point))))))
        ;; Rebind `minibuffer-message' called by `blink-matching-open'
        ;; to handle the overlay display.
        (cl-letf (((symbol-function #'minibuffer-message)
                   (lambda (msg &rest args)
                     (let ((msg (apply #'format-message msg args)))
                       (setq show-paren--off-screen-overlay
                             (display-line-overlay
                              (window-start) msg ))))))
          (blink-matching-open))))
    (advice-add #'show-paren-function :after #'show-paren-off-screen)))

(use-package highlight-parentheses
  :diminish highlight-parentheses-mode
  :config (global-highlight-parentheses-mode))

;; Highlight symbols
(use-package symbol-overlay
  :bind
  (("M-i"   . symbol-overlay-put)
   ("M-n"   . symbol-overlay-jump-next)
   ("M-p"   . symbol-overlay-jump-prev)
   ("M-N"   . symbol-overlay-switch-forward)
   ("M-P"   . symbol-overlay-switch-backward)
   ("M-C"   . symbol-overlay-remove-all)
   ([M-f3]  . symbol-overlay-remove-all))
  :diminish
  :functions (turn-off-symbol-overlay turn-on-symbol-overlay)
  :hook
  ((prog-mode . symbol-overlay-mode)
   ;; (iedit-mode . turn-off-symbol-overlay)
   ;; (iedit-mode-end . turn-on-symbol-overlay)
   )
  :custom
  (symbol-overlay-idle-time 0.1)
  :config
  ;; Disable symbol highlighting while selecting
  (defun turn-off-symbol-overlay (&rest _)
    "Turn off symbol highlighting."
    (interactive)
    (symbol-overlay-mode -1))
  (advice-add #'set-mark :after #'turn-off-symbol-overlay)

  (defun turn-on-symbol-overlay (&rest _)
    "Turn on symbol highlighting."
    (interactive)
    (when (derived-mode-p 'prog-mode)
      (symbol-overlay-mode 1)))
  (advice-add #'deactivate-mark :after #'turn-on-symbol-overlay)

  (use-package transient
    :bind
    (:map symbol-overlay-mode-map
          ("M-." . symbol-overlay-transient))
    :custom
    ((transient-levels-file (concat yc/cache-dir "/transient/levels.el"))
     (transient-values-file (concat yc/cache-dir "/transient/values.el"))
     (transient-history-file (concat yc/cache-dir "/transient/history.el")))
    :config
    (transient-define-prefix symbol-overlay-transient ()
      "Symbol Overlay transient"
      ["Symbol Overlay"
       ["Overlays"
        ("." "Add/Remove at point" symbol-overlay-put)
        ("k" "Remove All" symbol-overlay-remove-all)]
       ["Move to Symbol"
        ("n" "Next" symbol-overlay-switch-forward)
        ("p" "Previous" symbol-overlay-switch-backward)]
       ["Other"
        ("m" "Hightlight symbol-at-point" symbol-overlay-mode)
        ("w" "Copy symbol-at-point" symbol-overlay-save-symbol)]
       ]
      ))
  (defun symbol-overlay-switch-first ()
    (interactive)
    (let* ((symbol (symbol-overlay-get-symbol))
           (keyword (symbol-overlay-assoc symbol))
           (a-symbol (car keyword))
           (before (symbol-overlay-get-list a-symbol 'car))
           (count (length before)))
      (symbol-overlay-jump-call 'symbol-overlay-basic-jump (- count))))

  (defun symbol-overlay-switch-last ()
    (interactive)
    (let* ((symbol (symbol-overlay-get-symbol))
           (keyword (symbol-overlay-assoc symbol))
           (a-symbol (car keyword))
           (after (symbol-overlay-get-list a-symbol 'cdr))
           (count (length after)))
      (symbol-overlay-jump-call 'symbol-overlay-basic-jump (- count 1))))

  (setq symbol-overlay-map (make-sparse-keymap))
  (setq my/symbol-overlay-keymap (make-sparse-keymap))
  (define-key my/symbol-overlay-keymap (kbd "h") 'symbol-overlay-put)
  (define-key my/symbol-overlay-keymap (kbd "n") 'symbol-overlay-jump-next)
  (define-key my/symbol-overlay-keymap (kbd "p") 'symbol-overlay-jump-prev)
  (define-key my/symbol-overlay-keymap (kbd "w") 'symbol-overlay-save-symbol)
  (define-key my/symbol-overlay-keymap (kbd "t") 'symbol-overlay-toggle-in-scope)
  (define-key my/symbol-overlay-keymap (kbd "e") 'symbol-overlay-echo-mark)
  (define-key my/symbol-overlay-keymap (kbd "d") 'symbol-overlay-jump-to-definition)
  (define-key my/symbol-overlay-keymap (kbd "s") 'symbol-overlay-isearch-literally)
  (define-key my/symbol-overlay-keymap (kbd "q") 'symbol-overlay-query-replace)
  (define-key my/symbol-overlay-keymap (kbd "r") 'symbol-overlay-rename)
  (define-key my/symbol-overlay-keymap (kbd "<") 'symbol-overlay-switch-first)
  (define-key my/symbol-overlay-keymap (kbd ">") 'symbol-overlay-switch-last)
  ;; (global-set-key (kbd "C-c h") my/symbol-overlay-keymap)

  ;; 以下内容参考：https://gist.github.com/twlz0ne/2c743846bc73dd83c3f3e6a5cc85383e
  (define-advice symbol-overlay-put-one (:override (symbol &optional face) without-keymap)
    "Put overlay on current occurrence of SYMBOL after a match.
If FACE is non-nil, use it as the overlay’s face.
Otherwise apply `symbol-overlay-default-face'."
    (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
      (if face (progn (overlay-put ov 'face face)
		              ;; (overlay-put ov 'keymap symbol-overlay-map) ;; ---
		              (overlay-put ov 'evaporate t)
		              (overlay-put ov 'symbol symbol))
        (overlay-put ov 'face 'symbol-overlay-default-face)
        (overlay-put ov 'symbol ""))))

  (defun hilight--newest-overlay-symbol-at-point (pos)
    "Return symbol of newest overlay.
If there are multiple overlays at point, return the newest region overlay, skip the symbol overlay.
If there is only one overlay at point, just return it, no matter region or symbol."
    (interactive)
    (let ((symbols (--map (overlay-get it 'symbol)
                          (overlays-at pos))))
      (car (if (cdr symbols)
               (reverse
                (--filter (not (or (string-prefix-p "\\_<" it)
                                   (string-suffix-p "\\_>" it)))
                          symbols))
             symbols))))

  (defun hilight--jump (&optional backward)
    (interactive)
    (let* ((pos (point))
           (sym (hilight--newest-overlay-symbol-at-point pos))
           (testfn (if backward '< '>))
           (pointfn (if backward 'overlay-end 'overlay-start)))
      (catch 'break
        (--map
         (when (string= sym (overlay-get it 'symbol))
           (let ((dst (funcall pointfn it)))
             (when (funcall testfn dst pos)
               (goto-char (- dst (if backward 1 0)))
               (throw 'break nil))))
         (symbol-overlay-get-list)))))

  (defun hilight-toggle (&optional beg end)
    "Toggle all overlays of symbol or region at point."
    (interactive "r")
    (unless (minibufferp)
      (let* ((symbol (if (region-active-p)
                         (prog1 (buffer-substring-no-properties beg end)
                           (deactivate-mark))
                       (symbol-overlay-get-symbol)))
             (keyword (symbol-overlay-assoc symbol)))
        (if keyword
            (if (symbol-overlay-maybe-reput symbol keyword)
                (symbol-overlay-maybe-count keyword)
              (symbol-overlay-maybe-remove keyword)
              (symbol-overlay-maybe-put-temp))
          (and (looking-at-p "\\_>") (backward-char))
          (symbol-overlay-maybe-count
           (symbol-overlay-put-all symbol symbol-overlay-scope)
           t)))))

  (defun hilight-jump-next ()
    (interactive)
    (hilight--jump))

  (defun hilight-jump-prev ()
    (interactive)
    (hilight--jump t))

  ;; (defhydra hydra-hilight-jump (:hint nil)
  ;;   "\n <-- _p_rev _n_ext -->\n"
  ;;   ("n" hilight-jump-next)
  ;;   ("p" hilight-jump-prev))

  (defun hilight-jump-next+hydra ()
    (interactive)
    (hilight-jump-next)
    (hydra-hilight-jump/body))

  (defun hilight-jump-prev+hydra ()
    (interactive)
    (hilight-jump-prev)
    (hydra-hilight-jump/body))
  )

;; Highlight TODO and similar keywords in comments and strings
(use-package hl-todo
  :bind
  (:map hl-todo-mode-map
        ("C-c t p" . hl-todo-previous)
        ("C-c t n" . hl-todo-next)
        ("C-c t o" . hl-todo-occur))
  :hook
  (after-init . global-hl-todo-mode)
  :config
  (which-key-add-key-based-replacements "C-c t"   "hl-todo")
  (dolist (keyword '("BUG" "DEFECT" "ISSUE"))
    (cl-pushnew `(,keyword . ,(face-foreground 'error)) hl-todo-keyword-faces))
  (dolist (keyword '("WORKAROUND" "HACK" "TRICK"))
    (cl-pushnew `(,keyword . ,(face-foreground 'warning)) hl-todo-keyword-faces))

  (defvar-keymap hl-todo-repeat-map
    :repeat (:enter (hl-todo-insert) :exit (hl-todo-occur))
    "n" #'hl-todo-next
    "p" #'hl-todo-previous
    "o" #'hl-todo-occur))

;; *About:* 不同层级的代码块分隔符显示不同颜色
;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode)
  (cider-repl-mode . rainbow-delimiters-mode)
  :config
  (rainbow-delimiters-mode +1))

;; Colorize colors as text with their value.
;; 针对 #0000ff 显示可视化颜色
;; - https://elpa.gnu.org/packages/rainbow-mode.html
;; - 作用可以参考这篇文章：https://jblevins.org/log/rainbow-mode
(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode)
  :config
  (setq-default rainbow-x-colors-major-mode-list '())
  (with-no-warnings
    ;; HACK: Use overlay instead of text properties to override `hl-line' faces.
    ;; @see https://emacs.stackexchange.com/questions/36420
    (defun my-rainbow-colorize-match (color &optional match)
      (let* ((match (or match 0))
             (ov (make-overlay (match-beginning match) (match-end match))))
        (overlay-put ov 'ovrainbow t)
        (overlay-put ov 'face `((:foreground ,(if (> 0.5 (rainbow-x-color-luminance color))
                                                  "white" "black"))
                                (:background ,color)))))
    (advice-add #'rainbow-colorize-match :override #'my-rainbow-colorize-match)

    (defun my-rainbow-clear-overlays ()
      "Clear all rainbow overlays."
      (remove-overlays (point-min) (point-max) 'ovrainbow t))
    (advice-add #'rainbow-turn-off :after #'my-rainbow-clear-overlays)))

;; Pulse current line
(use-package pulse
  :ensure nil
  :custom-face
  (pulse-highlight-start-face ((t (:inherit region :background unspecified))))
  (pulse-highlight-face ((t (:inherit region :background unspecified :extend t))))
  :hook (((dumb-jump-after-jump
           imenu-after-jump) . my-recenter-and-pulse)
         ((bookmark-after-jump
           magit-diff-visit-file
           next-error) . my-recenter-and-pulse-line))
  :init
  (with-no-warnings
    (defun my-pulse-momentary-line (&rest _)
      "Pulse the current line."
      (pulse-momentary-highlight-one-line (point)))

    (defun my-pulse-momentary (&rest _)
      "Pulse the region or the current line."
      (if (fboundp 'xref-pulse-momentarily)
          (xref-pulse-momentarily)
        (my-pulse-momentary-line)))

    (defun my-recenter-and-pulse(&rest _)
      "Recenter and pulse the region or the current line."
      (recenter)
      (my-pulse-momentary))

    (defun my-recenter-and-pulse-line (&rest _)
      "Recenter and pulse the current line."
      (recenter)
      (my-pulse-momentary-line))

    (dolist (cmd '(recenter-top-bottom
                   other-window windmove-do-window-select
                   ace-window aw--select-window
                   winum-select-window-0
                   winum-select-window-1
                   winum-select-window-2
                   winum-select-window-3
                   winum-select-window-4
                   winum-select-window-5
                   winum-select-window-6
                   winum-select-window-7
                   winum-select-window-8
                   winum-select-window-9
                   winum-select-window-0-or-10
                   winum-select-window-by-number
                   pager-page-down pager-page-up
                   treemacs-select-window
                   symbol-overlay-basic-jump))
      (advice-add cmd :after #'my-pulse-momentary-line))

    (dolist (cmd '(pop-to-mark-command
                   pop-global-mark
                   goto-last-change))
      (advice-add cmd :after #'my-recenter-and-pulse))))

;; 作用是类似于 vim 里面，超过多少列时显示一个线
;; - [[file:screenshots/20210720_151451_VwrUcU.png]]
(use-package display-fill-column-indicator
  :ensure nil
  :hook
  (prog-mode . display-fill-column-indicator-mode)
  :custom
  ;; (display-fill-column-indicator-character ?▏)
  (display-fill-column-indicator-character ?¦)
  (display-fill-column-indicator-column 119)
  :init
  (setq-default fill-column 119)
  :config
  (defun aorst/display-fill-column-indicator-setup-faces ()
    (if (eq (frame-parameter nil 'background-mode) 'dark)
        (set-face-attribute 'fill-column-indicator nil
                            :foreground "gray30"
                            :distant-foreground "gray30"
                            :inherit nil)
      (set-face-attribute 'fill-column-indicator nil
                          :foreground "gray80"
                          :distant-foreground "gray80"
                          :inherit nil)))
  (aorst/display-fill-column-indicator-setup-faces))

;;================================================================================
(use-package highlight-symbol
  :disabled t
  :hook
  ((prog-mode . highlight-symbol-mode)
   (highlight-symbol-mode . highlight-symbol-nav-mode)
   ;; (org-mode . highlight-symbol-mode)
   )
  ;; :bind
  ;; (
  ;;   "M-<f3>" 'highlight-symbol-at-point
  ;;   "<f3>" 'highlight-symbol-next
  ;;   "s-<f3>" 'highlight-symbol-prev)
  :config
  (setq highlight-symbol-idle-delay 0.5)
  (highlight-symbol-mode t))

;; Visually highlight the selected buffer.
;; 高亮光标所在的 window
;; - https://github.com/gonewest818/dimmer.el
;; (use-package dimmer
;;   :init
;;   (dimmer-activate)
;;   :config
;;   (setq dimmer-percent 0.40)
;;   (dimmer-configure-hydra)
;;   (dimmer-configure-which-key)
;;   ;; (dimmer-configure-org)
;;   )

;; 当切换到不同的 buffer 时，会高亮当前光标所在的行
;; makes sure you don’t lose track of your cursor when jumping around a buffer.
;; - https://github.com/Malabarba/beacon
;; (use-package beacon
;;   :init
;;   (beacon-mode +1))
;;================================================================================

(provide 'init-highlight)
;;; init-highlight.el ends here
