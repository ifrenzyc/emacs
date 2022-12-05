;; init-term.el --- Initialize term configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; - http://ergoemacs.org/emacs/emacs_shell_vs_term_vs_ansi-term_vs_eshell.html
;; - https://www.masteringemacs.org/article/running-shells-in-emacs-overview
;; - http://songkun.me/2018/12/10/2018-12-10-emacs-run-shell/
;; - 参考这篇文章配置 multi-term：http://paralambda.org/2012/07/02/using-gnu-emacs-as-a-terminal-emulator/
;; - multi-term 下面几个烦人问题：https://blog.csdn.net/loveaborn/article/details/46854179
;; - https://github.com/aborn/multi-term-plus
;; 

;;; Code:

(setq-default shell-file-name "/usr/local/bin/zsh")

(use-package multi-term
  :disabled t
  :init
  (setq multi-term-program-switches "--login"
        system-uses-terminfo nil     ; Use Emacs terminfo, not system terminfo, mac系统出现了4m
        term-buffer-maximum-size 0)  ; 设置multi-term buffer的长度无限
  (setq multi-term-program "/usr/local/bin/zsh")
  (when (require 'multi-term nil t)
    (global-set-key (kbd "<C-next>") 'multi-term-next)
    (global-set-key (kbd "<C-prior>") 'multi-term-prev)
    (setq multi-term-buffer-name "term"))
  :config
  (when (require 'term nil t) ; only if term can be loaded..
    (setq term-bind-key-alist
          (list (cons "C-c C-c" 'term-interrupt-subjob)
                (cons "C-p" 'previous-line)
                (cons "C-n" 'next-line)
                (cons "M-f" 'term-send-forward-word)
                (cons "M-b" 'term-send-backward-word)
                (cons "C-c C-j" 'term-line-mode)
                (cons "C-c C-k" 'term-char-mode)
                (cons "M-DEL" 'term-send-backward-kill-word)
                (cons "M-d" 'term-send-forward-kill-word)
                (cons "<C-left>" 'term-send-backward-word)
                (cons "<C-right>" 'term-send-forward-word)
                (cons "C-r" 'term-send-reverse-search-history)
                (cons "M-p" 'term-send-raw-meta)
                (cons "M-y" 'term-send-raw-meta)
                (cons "C-y" 'term-send-raw))))
  ;;:bind ("<f5>" . multi-term)
  )

;; (use-package helm-mt
;;   :bind ("C-x t" . helm-mt))

;; (use-package better-shell
;;   :general
;;   (yc/nonprefix-keys
;;       "C-'" 'better-shell-shell
;;     "C-;" 'better-shell-remote-open))

(use-package shell-pop
  :disabled t
  :init
  (setq shell-pop-window-position "bottom"
        shell-pop-autocd-to-working-dir nil
        shell-pop-window-size     30
        ;; shell-pop-term-shell      "eshell"
        shell-pop-term-shell      "/usr/local/bin/zsh"
        ;; multi-term-program        "/usr/local/bin/zsh"
        shell-pop-shell-type '("term" "*vterminal*" (lambda () (multi-vterm)))
        ;; shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell)))
        ;; shell-pop-shell-type '("term" "*terminal*" (lambda () (ansi-term "/usr/local/bin/zsh" "*ansi-terminal*")))
        ;; shell-pop-shell-type '("term" "*terminal*" (lambda () (ansi-term "/usr/local/bin/zsh" "*ansi-terminal*")))
        ;; shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell))))
        shell-pop-full-span       t)
  
  :hook
  ((shell-mode . ansi-color-for-comint-mode-on)
   (term-mode . yc/ansi-term-handle-close)
   (term-mode . (lambda () (display-line-numbers-mode -1) (yas-minor-mode -1))))
  :config
  (defun yc/ansi-term-handle-close ()
    "Close current term buffer when `exit' from term buffer."
    (when (ignore-errors (get-buffer-process (current-buffer)))
      (set-process-sentinel (get-buffer-process (current-buffer))
                            (lambda (proc change)
                              (when (string-match "\\(finished\\|exited\\|exit\\)"
                                                  change)
                                (kill-buffer (process-buffer proc))
                                (when (> (count-windows) 1)
                                  (delete-window)))))))
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type)
  :general
  ("<f9>" 'shell-pop))

(provide 'init-term)
