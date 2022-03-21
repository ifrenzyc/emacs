;; init-highlight.el --- Initialize highlighting things configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code
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
(use-package highlight-symbol
  :disabled t
  :hook
  ((prog-mode . highlight-symbol-mode)
   (highlight-symbol-mode . highlight-symbol-nav-mode)
   ;; (org-mode . highlight-symbol-mode)
   )
  ;; :general
  ;; (yc/nonprefix-keys
  ;;   "M-<f3>" 'highlight-symbol-at-point
  ;;   "<f3>" 'highlight-symbol-next
  ;;   "s-<f3>" 'highlight-symbol-prev)
  :config
  (setq highlight-symbol-idle-delay 0.5)
  (highlight-symbol-mode t))

;; TODO 参考这个优化按键及显示方式
;; https://github.com/kaz-yos/emacs/blob/master/init.d/500_highlight-search-replace-related.el

;; Highlight matching parens
(use-package paren
  :straight (:type built-in)
  :hook (after-init . show-paren-mode)
  :init (setq show-paren-when-point-inside-paren t
              show-paren-when-point-in-periphery t)
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
  :diminish
  :functions (turn-off-symbol-overlay turn-on-symbol-overlay)
  :custom-face (symbol-overlay-default-face ((t (:inherit (region bold)))))
  :bind (("M-i" . symbol-overlay-put)
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)
         ("M-N" . symbol-overlay-switch-forward)
         ("M-P" . symbol-overlay-switch-backward)
         ("M-C" . symbol-overlay-remove-all)
         ("s-." . symbol-overlay-transient)
         ([M-f3] . symbol-overlay-remove-all))
  :hook ((prog-mode . symbol-overlay-mode)
         (iedit-mode . turn-off-symbol-overlay)
         (iedit-mode-end . turn-on-symbol-overlay))
  :init (setq symbol-overlay-idle-time 0.1)
  (with-eval-after-load 'all-the-icons
    (setq symbol-overlay-faces
          '((:inherit (all-the-icons-blue bold) :inverse-video t)
            (:inherit (all-the-icons-pink bold) :inverse-video t)
            (:inherit (all-the-icons-yellow bold) :inverse-video t)
            (:inherit (all-the-icons-purple bold) :inverse-video t)
            (:inherit (all-the-icons-red bold) :inverse-video t)
            (:inherit (all-the-icons-orange bold) :inverse-video t)
            (:inherit (all-the-icons-green bold) :inverse-video t)
            (:inherit (all-the-icons-cyan bold) :inverse-video t))))
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
  ;; (define-transient-command symbol-overlay-transient ()
  ;;  "Symbol Overlay transient"
  ;;  ["Symbol Overlay"
  ;;   ["Overlays"
  ;;    ("." "Add/Remove at point" symbol-overlay-put)
  ;;    ("k" "Remove All" symbol-overlay-remove-all)
  ;;    ]
  ;;   ["Move to Symbol"
  ;;    ("n" "Next" symbol-overlay-switch-forward)
  ;;    ("p" "Previous" symbol-overlay-switch-backward)
  ;;    ]
  ;;   ["Other"
  ;;    ("m" "Hightlight symbol-at-point" symbol-overlay-mode)
  ;;    ("w" "Copy symbol-at-point" symbol-overlay-save-symbol)
  ;;    ]
  ;;   ]
  ;; )
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

  (defhydra hydra-hilight-jump (:hint nil)
    "\n <-- _p_rev _n_ext -->\n"
    ("n" hilight-jump-next)
    ("p" hilight-jump-prev))

  (defun hilight-jump-next+hydra ()
    (interactive)
    (hilight-jump-next)
    (hydra-hilight-jump/body))

  (defun hilight-jump-prev+hydra ()
    (interactive)
    (hilight-jump-prev)
    (hydra-hilight-jump/body))

  (evil-leader/set-key
      "hh" 'hilight-toggle          ;; instead of `symbol-overlay-put'
    "hn" 'hilight-jump-next+hydra ;; instead of `symbol-overlay-jump-next'
    "hp" 'hilight-jump-prev+hydra ;; instead of `symbol-overlay-jump-prev'
    "ht" 'symbol-overlay-toggle-in-scope
    "ha" 'symbol-overlay-remove-all
    "he" 'symbol-overlay-echo-mark
    "hd" 'symbol-overlay-jump-to-definition
    "hs" 'symbol-overlay-isearch-literally
    "hq" 'symbol-overlay-query-replace
    "hr" 'symbol-overlay-rename)
  )

;; (use-package highlight-thing
;;   :config
;;   (add-hook 'iedit-mode-hook (lambda()
;; 			                   (highlight-thing-mode -1)))

;;   (add-hook 'iedit-mode-end-hook (lambda()
;; 				                   (highlight-thing-mode 1)))
;;   (add-hook 'evil-visual-state-entry-hook (lambda()
;; 					                        (highlight-thing-mode -1)))
;;   (add-hook 'evil-visual-state-exit-hook (lambda()
;; 					                       (highlight-thing-mode 1)))
;;   :init
;;   (setq highlight-thing-what-thing 'symbol
;; 	    highlight-thing-delay-seconds 0.5
;; 	    highlight-thing-all-visible-buffers-p nil
;; 	    highlight-thing-limit-to-region-in-large-buffers-p t
;; 	    highlight-thing-narrow-region-lines 30)
;;   :hook
;;   ((org-mode csv-mode) . (lambda()
;; 		                   (highlight-thing-mode 0)))
;;   (prog-mode . highlight-thing-mode))

;; Highlight TODO and similar keywords in comments and strings
(use-package hl-todo
  :bind (:map hl-todo-mode-map
              ([C-f3] . hl-todo-occur)
              ("C-c t p" . hl-todo-previous)
              ("C-c t n" . hl-todo-next)
              ("C-c t o" . hl-todo-occur))
  :hook (after-init . global-hl-todo-mode)
  :config
  (dolist (keyword '("BUG" "DEFECT" "ISSUE"))
    (cl-pushnew `(,keyword . ,(face-foreground 'error)) hl-todo-keyword-faces))
  (dolist (keyword '("WORKAROUND" "HACK" "TRICK"))
    (cl-pushnew `(,keyword . ,(face-foreground 'warning)) hl-todo-keyword-faces)))

;; Highlight uncommitted changes using VC
;; 高亮显示未提交的代码块
;; 
;; 参考：采用 hook 方式解决：参考 leuven 的配置，解决 magit commit 后，当前打开的文件 diff-hl 还显示文件变更差异
;; file:/Users/yangc/src/emacs.d/emacs-leuven/emacs-leuven.txt::2705
(use-package diff-hl
  :init
  (setq diff-hl-draw-borders nil)
  (setq diff-hl-side 'left)
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :config
  (diff-hl-flydiff-mode t)
  ;; Integration with magit
  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))
  ;; (custom-set-faces
  ;;  '(diff-hl-change ((t (:background "#3a81c3"))))
  ;;  '(diff-hl-insert ((t (:background "#7ccd7c"))))
  ;;  '(diff-hl-delete ((t (:background "#ee6363")))))
  )

;; - https://github.com/antonj/Highlight-Indentation-for-Emacs
;; - https://github.com/DarthFennec/highlight-indent-guides
(use-package highlight-indentation
  ;; :hook (prog-mode . highlight-indentation-mode)
  ;; :config
  ;; (set-face-background 'highlight-indentation-face "#e3e3d3")
  ;; (set-face-background 'highlight-indentation-current-column-face "#c3b3b3")
  )

(use-package highlight-indent-guides
  :disabled t
  ;; :hook (prog-mode . highlight-indent-guides-mode)  ; 默认不启动这个，非常影响性能
  :functions (macrostep-expand macrostep-collapse)
  :init
  (setq highlight-indent-guides-auto-character-face-perc 25
        ;; highlight-indent-guides-character ?|
        ;; highlight-indent-guides-character ?❚
		;; highlight-indent-guides-character ?‖
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-method 'character)  ;; 'column 'character 'bitmap

  ;; (set-face-background 'highlight-indent-guides-odd-face "darkgray")
  ;; (set-face-background 'highlight-indent-guides-even-face "dimgray")
  ;; (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
  :config
  ;; WORKAROUND: Reset the faces after changing theme
  (add-hook 'after-load-theme-hook
            (lambda ()
              "Re-render indentations after changing theme."
              (when highlight-indent-guides-mode
                (highlight-indent-guides-auto-set-faces))))

  ;; Don't display first level of indentation
  (defun my-indent-guides-for-all-but-first-column (level responsive display)
    (unless (< level 1)
      (highlight-indent-guides--highlighter-default level responsive display)))
  (setq highlight-indent-guides-highlighter-function
        #'my-indent-guides-for-all-but-first-column)

  ;; Don't display indentations in `swiper'
  ;; https://github.com/DarthFennec/highlight-indent-guides/issues/40
  (with-eval-after-load 'ivy
    (defun my-ivy-cleanup-indentation (str)
      "Clean up indentation highlighting in ivy minibuffer."
      (let ((pos 0)
            (next 0)
            (limit (length str))
            (prop 'highlight-indent-guides-prop))
        (while (and pos next)
          (setq next (text-property-not-all pos limit prop nil str))
          (when next
            (setq pos (text-property-any next limit prop nil str))
            (ignore-errors
              (remove-text-properties next pos '(display nil face nil) str))))))
    (advice-add #'ivy-cleanup-string :after #'my-ivy-cleanup-indentation))
  )
;; (eval-after-load 'highlight-indent-guides
;;   (custom-set-faces
;;    '(highlight-indent-guides-top-character-face ((t (:foreground "turquoise1")))))
;;   )

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

;; Pulse current line
(use-package pulse
  :straight (:type built-in)
  :custom-face
  (pulse-highlight-start-face ((t (:inherit region))))
  (pulse-highlight-face ((t (:inherit region))))
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
  :straight (:type built-in)
  :hook
  (prog-mode . display-fill-column-indicator-mode)
  :custom
  ;; (display-fill-column-indicator-character ?▏)
  (display-fill-column-indicator-character ?¦)
  (display-fill-column-indicator-column 119)
  :init
  (setq-default fill-column  119)
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

(provide 'init-highlight)
