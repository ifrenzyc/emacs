;; init-basic.el --- Initialize basic settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:
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
      default-file-name-coding-system 'utf-8)
      ;; default-buffer-file-coding-system 'utf-8

(modify-coding-system-alist 'process "*" 'utf-8)

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(use-package emacs
  :ensure nil
  :custom (request-storage-directory (concat yc/cache-dir "/request"))
  :config
  ;; 改变 Emacs 要你回答 yes 的行为。按 y 或空格键表示 yes，n 表示 no。
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; 光标靠近鼠标指针时，让鼠标指针自动让开，别挡住视线。
  (setq mouse-avoidance-mode 'animate)

  (setq visible-bell t)
  (setq ring-bell-function 'ignore) ; visible-bell doesn’t work well on OS X, so disable those notifications completely

  (setq inhibit-compacting-font-caches t) ; Don’t compact font caches during GC.

  (setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same

  ;; 在 modeline 上显示行列号
  (column-number-mode 1)
  (line-number-mode 1)
  (size-indication-mode t)

  ;; make cursor the width of the character it is under
  ;; i.e. full width of a TAB
  (setq x-stretch-cursor t)

  (setq-default cursor-in-non-selected-windows nil)

  ;; (global-visual-line-mode t)  ;; this enable beginning-of-visual-line to remap move-beginning-of-line
  ;; (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
  (global-hl-line-mode 0)
  (setq word-wrap t)
  (setq truncate-lines nil)
  (set-fill-column 89)
  (auto-fill-mode t)                    ; 自动将内容换行

  (setq line-move-visual nil)
  (setq track-eol t)                    ; Keep cursor at end of lines. Require line-move-visual is nil.

  ;; 在行首 C-k 时，同时删除该行
  (setq-default kill-whole-line t)

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
  (setq backward-delete-char-untabify-method 'hungry))

(provide 'init-basic)
;;; init-basic.el ends here
