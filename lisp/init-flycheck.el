;; init-flycheck.el --- Initialize flycheck settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:
(use-package flycheck
  :commands global-flycheck-mode
  :bind
  (:map flycheck-error-list-mode-map
        ("RET" . flycheck-error-list-goto-error)
        ("j"   . flycheck-error-list-next-error)
        ("k"   . flycheck-error-list-previous-error)
        ("q"   . quit-window))
  :pretty-hydra
  (hydra-flycheck
   (:hint nil :color teal :quit-key "C-g" :title (with-faicon "nf-fa-plane" "Flycheck" 1 -0.05))
   ("Current error"
    (("e" flycheck-explain-error-at-point "explain")
     ("c" flycheck-copy-errors-as-kill "copy"))
    "Checker"
    (("?" flycheck-describe-checker "describe")
     ("d" flycheck-disable-checker "disable")
     ("m" flycheck-mode "mode")
     ("s" flycheck-select-checker "select"))
    "Errors"
    (("k" flycheck-previous-error "previous" :color pink)
     ("j" flycheck-next-error "next" :color pink)
     ("f" flycheck-buffer "check")
     ("l" flycheck-list-errors "list")
     ("L" flycheck-projectile-list-errors "list project"))
    "Other"
    (("M" flycheck-manual "manual")
     ("v" flycheck-verify-setup "verify setup"))))
  :hook
  (after-init . global-flycheck-mode)
  :init
  (setq flycheck-global-modes
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
  :config
  ;; (defun my/toggle-syntax-checking ()
  ;;   (interactive)
  ;;   (if (bound-and-true-p flycheck-mode)
  ;;       (progn
  ;;         (flycheck-mode -1)
  ;;         (message "Flycheck mode disabled in current buffer"))
  ;;     (progn
  ;;       (flycheck-mode 1)
  ;;       (message "Flycheck mode enabled in current buffer"))))

  (progn (use-package popup)
         (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc)))

;; - https://github.com/gexplorer/flycheck-indicator
(use-package flycheck-indicator
  :hook (flycheck-mode . flycheck-indicator-mode))

(use-package flycheck-pos-tip)

(use-package flycheck-popup-tip
  :hook (flycheck-mode . flycheck-popup-tip-mode))

(use-package flycheck-inline
  :hook (flycheck-mode . flycheck-inline-mode))

(use-package flycheck-posframe
  :hook
  (flycheck-mode . flycheck-posframe-mode)
  :custom-face
  (flycheck-posframe-error-face ((t (:background "DarkSlateBlue"))))
  (flycheck-posframe-warning-face ((t (:background "DarkSlateBlue"))))
  (flycheck-posframe-border-face ((t (:background "DarkBlue"))))
  :config
  (setq flycheck-posframe-border-width 1)
  (flycheck-posframe-configure-pretty-defaults))

;;================================================================================
(use-package sideline-flycheck
  :disabled t
  :hook
  ((flycheck-mode . sideline-flycheck-setup)
   (flycheck-mode . sideline-mode)))
;;================================================================================

(provide 'init-flycheck)
;;; init-flycheck.el ends here
