;; lang-basic.el --- Initialize Language basic settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:

;; Show native line numbers if possible, otherwise use `linum'
(use-package display-line-numbers
  :ensure nil
  :hook
  (prog-mode . display-line-numbers-mode)
  :custom
  (display-line-numbers-width-start t))

;; custom program language font
(setq yc/programming-modes '(js-mode
                             python-mode
                             python-ts-mode
                             html-mode
                             web-mode
                             vue-html-mode
                             java-mode
                             c-mode
                             css-mode
                             ruby-mode
                             sh-mode
                             fish-mode
                             bat-mode
                             go-mode
                             js-mode
                             lua-mode
                             lua-ts-mode
                             php-mode
                             perl-mode
                             systemd-mode
                             stylus-mode
                             json-mode
                             jsonian-mode
                             dockerfile-mode
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
                             applescript-mode
                             emacs-lisp-mode
                             lisp-interaction-mode
                             protobuf-mode
                             bash-ts-mode
                             c++-ts-mode
                             c-ts-mode
                             c-or-c++-ts-mode
                             cmake-ts-mode
                             csharp-ts-mode
                             css-ts-mode
                             dockerfile-ts-mode
                             docker-compose-mode
                             go-mod-ts-mode
                             go-ts-mode
                             java-ts-mode
                             js-ts-mode
                             json-ts-mode
                             ng2-ts-mode
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
  :custom
  (xref-show-definitions-function #'xref-show-definitions-completing-read)
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-selector 'popup)    ; helm, ivy
  :hook
  (xref-backend-functions . dumb-jump-xref-activate))

(use-package dap-mode
  :after lsp
  :bind
  (:map dap-mode-map
        ([f5]  . dap-debug)
        ([f7]  . dap-hydra))
  :hook
  ((prog-mode . dap-mode)
   (dap-mode . dap-ui-mode)
   (dap-session-created . (lambda (&_rest) (dap-hydra)))
   (dap-terminated . (lambda (&_rest) (dap-hydra/nil)))
   (dap-stopped    . (lambda (arg) (call-interactively #'dap-hydra)))   ; automatically trigger the hydra when the program hits a breakpoint
   (python-ts-mode . (lambda () (require 'dap-python)))
   (ruby-ts-mode   . (lambda () (require 'dap-ruby)))
   (go-ts-mode     . (lambda () (require 'dap-go)))
   (java-ts-mode   . (lambda () (require 'dap-java)))
   ((c-mode c-ts-mode c++-mode c++-ts-mode objc-mode swift) . (lambda () (require 'dap-lldb)))
   (php-mode    . (lambda () (require 'dap-php)))
   (elixir-mode . (lambda () (require 'dap-elixir)))
   ((js-mode js-ts-mode js2-mode) . (lambda () (require 'dap-chrome))))
  :custom
  (dap-java-test-runner (concat lsp-java-server-install-dir "test-runner/junit-platform-console-standalone.jar"))
  (dap-breakpoints-file (concat yc/cache-dir ".dap-breakpoints"))
  :config
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
  (dap-ui-controls-mode 1))

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
  ;;:commands (sidekick-at-point sidekick-focus-toggle sidekick-search-for-literal)
  )

(use-package indent-bars
  :init
  (require 'indent-bars-ts)
  :custom-face
  (indent-bars-face ((t (:height 1.08))))
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  :hook
  ((python-mode python-ts-mode
                yaml-mode yaml-ts-mode
                dockerfile-mode dockerfile-ts-mode
                nxml-mode) . indent-bars-mode))

;;================================================================================
;; shows a sticky header at the top of the window
;; (use-package topsy
;;   :hook (prog-mode . topsy-mode))

;; (use-package ggtags
;;   :ensure-system-package (yapf . "brew install global ctags")
;;   :config
;;   (add-hook 'c-mode-common-hook
;; 	        (lambda ()
;; 	          (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
;; 		        (ggtags-mode 1))))

;;   (setq ggtags-global-ignore-case t
;;         ggtags-sort-by-nearness t))

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
;; - https://github.com/lassik/emacs-format-all-the-code

;; - https://github.com/antonj/Highlight-Indentation-for-Emacs
;; - https://github.com/DarthFennec/highlight-indent-guides
;; - https://github.com/jdtsmith/indent-bars
(use-package highlight-indentation
  :disabled t
  :hook
  (prog-mode . highlight-indentation-mode)
  :config
  (set-face-background 'highlight-indentation-face "#e3e3d3")
  (set-face-background 'highlight-indentation-current-column-face "#c3b3b3"))

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
    (advice-add #'ivy-cleanup-string :after #'my-ivy-cleanup-indentation)))
;;================================================================================

(provide 'lang-basic)
;;; lang-basic.el ends here
