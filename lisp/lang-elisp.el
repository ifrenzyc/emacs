;; lang-elisp.el --- Initialize elisp settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:
(use-package emacs-lisp-mode
  :ensure nil
  :mode ("\\.el\\'" . emacs-lisp-mode)
  :mode-hydra
  (emacs-lisp-mode
   (:title "Emacs Lisp Commands")
   ("Eval"
    (("b" eval-buffer "buffer")
     ("e" eval-defun "defun")
     ("r" eval-region "region"))
    "REPL"
    (("I" ielm "ielm"))
    "Test"
    (("t" ert "prompt")
     ("T" (ert t) "all")
     ("F" (ert :failed) "failed"))
    "Doc"
    (("d" helpful-at-point "thing-at-point")
     ("f" describe-function "function")
     ("v" describe-variable "variable")
     ("i" info-lookup-symbol "info lookup")))))

(use-package eldoc
  :ensure nil
  :diminish
  :delight eldoc-mode
  :custom
  ((eldoc-idle-delay 0.1)
   (eldoc-echo-area-use-multiline-p nil))
  :config
  ;; Display `eldoc' in child frame
  (with-no-warnings
    (defvar eldoc-posframe-buffer "*eldoc-posframe-buffer*"
      "The posframe buffer name use by eldoc-posframe.")

    (defvar eldoc-posframe-hide-posframe-hooks
      '(pre-command-hook post-command-hook focus-out-hook)
      "The hooks which should trigger automatic removal of the posframe.")

    (defvar eldoc-posframe-delay 0.2
      "Delay seconds to display `eldoc'.")

    (defvar-local eldoc-posframe--timer nil)

    (defun eldoc-posframe-hide-posframe ()
      "Hide messages currently being shown if any."
      (when eldoc-posframe--timer
        (cancel-timer eldoc-posframe--timer))

      (dolist (hook eldoc-posframe-hide-posframe-hooks)
        (remove-hook hook #'eldoc-posframe-hide-posframe t)))

    (defun eldoc-posframe-show-posframe (str &rest args)
      "Display STR with ARGS."
      (when eldoc-posframe--timer
        (cancel-timer eldoc-posframe--timer))

      (posframe-hide eldoc-posframe-buffer)
      (dolist (hook eldoc-posframe-hide-posframe-hooks)
        (add-hook hook #'eldoc-posframe-hide-posframe nil t))

      (setq eldoc-posframe--timer
            (run-with-idle-timer
             eldoc-posframe-delay nil
             (lambda ()
               (when str
                 (posframe-show
                  eldoc-posframe-buffer
                  :string (apply #'format str args)
                  :postion (point)
                  :poshandler #'posframe-poshandler-point-bottom-left-corner-upward
                  :left-fringe 8
                  :right-fringe 8
                  :internal-border-width 1
                  :internal-border-color (face-attribute 'font-lock-comment-face :foreground)
                  :background-color (face-background 'tooltip)))))))
    (add-hook 'emacs-lisp-mode-hook
              (lambda ()
                (setq-local eldoc-message-function #'eldoc-posframe-show-posframe)))))

(use-package eldoc-box
  :after eldoc
  :custom
  (eldoc-box-clear-with-C-g t)
  :hook
  ((eldoc-mode . eldoc-box-hover-mode)
   (eldoc-mode . eldoc-box-hover-at-point-mode)))

(use-package cl-indent
  :ensure nil
  :config
  (setq lisp-indent-function #'common-lisp-indent-function)
  (defvar cm/correct-indentation-list
    '((defface . nil)
      (defalias . nil)
      (define-minor-mode . 1)
      (define-derived-mode . 3)
      (defface . 1)
      ;; (unwind-protect . 1)
      (define-globalized-minor-mode . nil)
      ;; Fix `use-pacakge' indentation.
      (use-package . 1)))
  (pcase-dolist (`(,sym . ,spec) cm/correct-indentation-list)
    (put sym 'common-lisp-indent-function-for-elisp spec)))

;; Here are packages that are useful across different Lisp and Scheme implementations.
;; (use-package lispy
;;   :hook ((emacs-lisp-mode . lispy-mode)
;;          (scheme-mode . lispy-mode)))

;; (use-package lispyville
;;   :hook ((lispy-mode . lispyville-mode))
;;   :config
;;   (lispyville-set-key-theme '(operators c-w additional
;;                                         additional-movement slurp/barf-cp
;;                                         prettify)))

(provide 'lang-elisp)
;;; lang-elisp.el ends here
