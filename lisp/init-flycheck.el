;; init-flycheck.el --- Initialize flycheck settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code
(use-package flycheck
  :commands global-flycheck-mode
  :hook (after-init . global-flycheck-mode)
  :init (setq flycheck-global-modes
              '(not text-mode outline-mode fundamental-mode lisp-interaction-mode
                org-mode diff-mode shell-mode eshell-mode term-mode vterm-mode)
              flycheck-emacs-lisp-load-path 'inherit
              flycheck-display-errors-function 'flycheck-pos-tip-error-messages
              flycheck-indication-mode (if (display-graphic-p)
                                           'right-fringe
                                         'right-margin)
              ;; Only check while saving and opening files
              flycheck-check-syntax-automatically '(save mode-enabled))
  (setenv "DICTIONARY" "en_US")
  :custom
  (flyspell-abbrev-p t)
  :general
  (yc/leader-keys
      "ts" 'my/toggle-syntax-checking
    "eb" 'flycheck-buffer
    "ec" 'flycheck-clear
    "eh" 'flycheck-describe-checker
    "es" 'flycheck-select-checker
    "eS" 'flycheck-set-checker-executable
    "ev" 'flycheck-verify-setup
    "ey" 'flycheck-copy-errors-as-kill
    "ex" 'flycheck-explain-error-at-point
    ;; navigation
    "en" 'flycheck-next-error
    "ep" 'flycheck-previous-error
    "el" 'flycheck-list-errors
    "e." 'hydra-flycheck/body)
  (yc/nonprefix-keys
      :keymaps 'flycheck-error-list-mode-map
    "RET" 'flycheck-error-list-goto-error
    "j" 'flycheck-error-list-next-error
    "k" 'flycheck-error-list-previous-error
    "q" 'quit-window)
  :config
  (defun my/toggle-syntax-checking ()
    (interactive)
    (if (bound-and-true-p flycheck-mode)
        (progn
          (flycheck-mode -1)
          (message "Flycheck mode disabled in current buffer"))
      (progn
        (flycheck-mode 1)
        (message "Flycheck mode enabled in current buffer"))))

  (progn (use-package popup)
         (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc))

  (defhydra hydra-flycheck ()
    "errors"
    ("n" flycheck-next-error "next")
    ("p" flycheck-previous-error "previous")
    ;; ("h" helm-flycheck "helm" :color blue)
    ("q" nil "quit")))

;; - https://github.com/gexplorer/flycheck-indicator
(use-package flycheck-indicator
  :hook (flycheck-mode . flycheck-indicator-mode))

(use-package flycheck-pos-tip)

(use-package flycheck-popup-tip
  :hook (flycheck-mode . flycheck-popup-tip-mode))

(use-package flycheck-inline
  :hook (flycheck-mode . flycheck-inline-mode))

(use-package flycheck-posframe
  ;; :straight (:host github :repo "alexmurray/flycheck-posframe")
  :hook (flycheck-mode . flycheck-posframe-mode)
  :custom-face (flycheck-posframe-error-face ((t (:background "DarkSlateBlue"))))
  :custom-face (flycheck-posframe-warning-face ((t (:background "DarkSlateBlue"))))
  :custom-face (flycheck-posframe-border-face ((t (:background "DarkBlue"))))
  :config
  (setq flycheck-posframe-border-width 1)
  (flycheck-posframe-configure-pretty-defaults))

(provide 'init-flycheck)