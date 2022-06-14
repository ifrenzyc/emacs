;; init-basic.el --- Initialize basic settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code
;; Set the default encoding system, use `UTF-8` for env.
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-clipboard-coding-system 'utf-8)

(setq locale-coding-system 'utf-8
      buffer-file-coding-system 'utf-8
      default-file-name-coding-system 'utf-8
      default-buffer-file-coding-system 'utf-8)

(modify-coding-system-alist 'process "*" 'utf-8)

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(use-package emacs
  :ensure nil
  :config
  ;; 改变 Emacs 要你回答 yes 的行为。按 y 或空格键表示 yes，n 表示 no。
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; 不显示行号
  (setq linum-mode nil
        global-linum-mode nil)

  ;; 打开括号匹配显示模式
  (show-paren-mode t)

  ;; 括号匹配时可以高亮显示另外一边的括号，但光标不会烦人的跳到另一个括号处。
  (setq show-paren-style 'parenthesis)

  ;; 光标靠近鼠标指针时，让鼠标指针自动让开，别挡住视线。
  (setq mouse-avoidance-mode 'animate)

  (setq visible-bell t)
  (setq ring-bell-function 'ignore) ; visible-bell doesn’t work well on OS X, so disable those notifications completely

  (setq inhibit-compacting-font-caches t) ; Don’t compact font caches during GC.
  (setq delete-by-moving-to-trash t)      ; Deleting files go to OS's trash folder

  (setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same

  ;; 在 modeline 上显示行列号
  (column-number-mode 1)
  (line-number-mode 1)
  (size-indication-mode t)
  ;; (set-fringe-style '(4 . 4))

  ;; make cursor the width of the character it is under
  ;; i.e. full width of a TAB
  (setq x-stretch-cursor t)

  (setq-default cursor-in-non-selected-windows nil)

  (global-visual-line-mode t)
  (setq word-wrap t)
  (setq truncate-lines nil)
  (set-fill-column 89)
  (auto-fill-mode t)                    ; 自动将内容换行

  (setq line-move-visual nil)
  (setq track-eol t)                    ; Keep cursor at end of lines. Require line-move-visual is nil.

  ;; respect ansi colors
  ;; (ansi-color-for-comint-mode-on)

  ;; ansi colors in compilation mode
  ;; (ignore-errors
  ;;   (defun yc/colorize-compilation-buffer ()
  ;;     (when (eq major-mode 'compilation-mode)
  ;;       (ansi-color-apply-on-region compilation-filter-start (point-max))))
  ;;   (add-hook 'compilation-filter-hook yc/colorize-compilation-buffer))

  ;; 在行首 C-k 时，同时删除该行
  (setq-default kill-whole-line t)

  (global-hl-line-mode 0)

  ;; ignore byte-compile warnings
  ;; (setq byte-compile-warnings (set-difference byte-compile-warning-types
  ;;                                             '(free-vars
  ;;                                               unresolved
  ;;                                               callargs
  ;;                                               redefine
  ;;                                               obsolete
  ;;                                               noruntime
  ;;                                               cl-functions
  ;;                                               interactive-only)))
  (setq warning-minimum-level :error))

;; - https://dougie.io/coding/tabs-in-emacs/
;; In Emacs, sentences uses double-spaces by default. Use single spaces instead:
(setq sentence-end-double-space nil)

;; Disable tabs and set prefered indentation width in spaces
;; (In this case the indent size is 2-spaces wide)
(setq-default indent-tabs-mode nil)
(setq-default standard-indent 4)
(setq-default tab-width 4)
(setq-default js-indent-width 4)

;; Make the backspace properly erase the tab instead of
;; removing 1 space at a time.
(setq backward-delete-char-untabify-method 'hungry)

(provide 'init-basic)
