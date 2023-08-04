;; lang-basic.el --- Initialize Language basic settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:

;; Show native line numbers if possible, otherwise use `linum'
(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode)
  :config
  (setq display-line-numbers-width-start t))

;; custom program language font
(setq yc/programming-modes '(js-mode
                             python-mode
                             html-mode
                             web-mode
                             vue-html-mode
                             java-mode
                             c-mode
                             css-mode
                             ruby-mode
                             sh-mode
                             fish-mode
                             go-mode
                             js-mode
                             lua-mode
                             stylus-mode
                             json-mode
                             jsonian-mode
                             makefile-mode
                             jenkinsfile-mode
                             nxml-mode
                             yaml-mode
                             sql-mode
                             groovy-mode
                             kotlin-mode
                             objc-mode
                             racket-mode
                             conf-mode
                             fundamental-mode
                             emacs-lisp-mode
                             lisp-interaction-mode
                             bash-ts-mode
                             c++-ts-mode
                             c-ts-mode
                             c-or-c++-ts-mode
                             cmake-ts-mode
                             csharp-ts-mode
                             css-ts-mode
                             dockerfile-ts-mode
                             go-mod-ts-mode
                             go-ts-mode
                             java-ts-mode
                             js-ts-mode
                             json-ts-mode
                             ng2-ts-mode
                             python-ts-mode
                             ruby-ts-mode
                             rust-ts-mode
                             toml-ts-mode
                             tsx-ts-mode
                             typescript-ts-mode
                             yaml-ts-mode))

(defun yc/apply-monofont ()
  (interactive)
  ;; (setq buffer-face-mode-face '(:family "Anonymous Pro" :height 160))
  (setq buffer-face-mode-face '(:family "IBM Plex Mono" :weight regular :height 130))
  (setq-local line-spacing 0.2)
  (buffer-face-mode t))

(require 'derived)
(dolist (yc/mode yc/programming-modes)
  (add-hook (derived-mode-hook-name yc/mode) 'yc/apply-monofont))

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config
  (setq dumb-jump-selector 'ivy)  ;; 'helm

  (defhydra hydra-dumb-jump (:color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back")))

(use-package dap-mode
  :after lsp
  :general (dap-mode-map
            "<f5>"  'dap-debug
            "<f7>"  'dap-hydra)
  :hook ((prog-mode . dap-mode)
         (dap-mode . dap-ui-mode)
         (dap-session-created . (lambda (&_rest) (dap-hydra)))
         (dap-terminated . (lambda (&_rest) (dap-hydra/nil)))
         (python-ts-mode . (lambda () (require 'dap-python)))
         (ruby-ts-mode . (lambda () (require 'dap-ruby)))
         (go-ts-mode . (lambda () (require 'dap-go)))
         (java-ts-mode . (lambda () (require 'dap-java)))
         ((c-mode c-ts-mode c++-mode c++-ts-mode objc-mode swift) . (lambda () (require 'dap-lldb)))
         (php-mode . (lambda () (require 'dap-php)))
         (elixir-mode . (lambda () (require 'dap-elixir)))
         ((js-mode js-ts-mode js2-mode) . (lambda () (require 'dap-chrome))))
  :config
  (setq dap-java-test-runner (concat lsp-java-server-install-dir "test-runner/junit-platform-console-standalone.jar"))
  (setq dap-breakpoints-file (concat yc/cache-dir ".dap-breakpoints"))
  (dap-auto-configure-mode t)
  (dap-mode 1)

  ;; The modes below are optional
  (dap-ui-mode 1)
  ;; enables mouse hover support
  (dap-tooltip-mode 1)
  ;; use tooltips for mouse hover
  ;; if it is not enabled `dap-mode' will use the minibuffer.
  (tooltip-mode 1)
  ;; displays floating panel with debug buttons
  ;; requies emacs 26+
  (dap-ui-controls-mode 1)
  ;; automatically trigger the hydra when the program hits a breakpoint
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra))))

;; (use-package ggtags
;;   :ensure-system-package (yapf . "brew install global ctags")
;;   :config
;;   (add-hook 'c-mode-common-hook
;; 	        (lambda ()
;; 	          (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
;; 		        (ggtags-mode 1))))

;;   (setq ggtags-global-ignore-case t
;;         ggtags-sort-by-nearness t))

;; Minor mode to aggressively keep your code always indented
(use-package aggressive-indent
  :diminish
  :hook ((prog-mode . global-aggressive-indent-mode)
         ;; FIXME: Disable in big files due to the performance issues
         ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
         (find-file . (lambda ()
                        (if (> (buffer-size) (* 3000 80))
                            (aggressive-indent-mode -1)))))
  :config
  ;; Disable in some modes
  (dolist (mode '(asm-mode web-mode html-mode css-mode java-mode java-ts-mode go-mode go-ts-mode scala-mode prolog-inferior-mode))
    (push mode aggressive-indent-excluded-modes))

  ;; Disable in some commands
  (add-to-list 'aggressive-indent-protected-commands #'delete-trailing-whitespace t)

  ;; Be slightly less aggressive in C/C++/C#/Java/Go/Swift
  (add-to-list 'aggressive-indent-dont-indent-if
               '(and (derived-mode-p 'c-mode 'c-ts-mode 'c++-mode 'c++-ts-mode 'csharp-mode 'csharp-ts-mode
                                     'java-mode 'java-ts-mode 'go-mode 'go-ts-mode 'swift-mode)
                     (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                                         (thing-at-point 'line))))))

;; @see - https://github.com/VernonGrant/sidekick.el
(use-package sidekick
  :load-path "localelpa/sidekick"
  :commands (sidekick-at-point sidekick-focus-toggle sidekick-search-for-literal))

;; shows a sticky header at the top of the window
;; (use-package topsy
;;   :hook (prog-mode . topsy-mode))

(use-package which-func
  :disabled t
  :ensure nil
  :config
  (setq which-func-unknown "n/a")
  ;; Show the current function name in the header line
  (setq-default header-line-format
                '((which-func-mode ("" which-func-format " "))))
  (setq mode-line-misc-info
        ;; We remove Which Function Mode from the mode line, because it's mostly
        ;; invisible here anyway.
        (assq-delete-all 'which-func-mode mode-line-misc-info))
  (which-function-mode t)
  (add-to-list 'which-func-modes 'prog-mode)
  (add-to-list 'which-func-modes 'java-ts-mode)
  (add-to-list 'which-func-modes 'ptyhon-ts-mode)
  (add-to-list 'which-func-modes 'go-ts-mode)
  (add-to-list 'which-func-modes 'emacs-lisp-mode))

;; code format
;; - https://github.com/radian-software/apheleia

(provide 'lang-basic)
;;; lang-basic.el ends here
