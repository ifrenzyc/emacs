;; init-which-key.el --- Initialize which-key settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; Displays available keybindings in popup.
;; - https://github.com/justbur/emacs-which-key
;; 

;;; Code:
(use-package which-key
  :hook
  (after-init . which-key-mode)
  :custom
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-use-C-h-commands t)
  (which-key-idle-delay 0.8)
  :config
  (which-key-setup-minibuffer)
  (which-key-setup-side-window-bottom))

;; https://tildegit.org/acdw/define-repeat-map.el
;; https://karthinks.com/software/persistent-prefix-keymaps-in-emacs/
(use-package repeat-help
  :hook (repeat-mode . repeat-help-mode)
  :init
  (repeat-mode t))

;; https://github.com/tarsius/keycast
(use-package keycast
  :disabled t
  :commands keycast-mode
  :config
  ;; This works with doom-modeline, inspired by this comment:
  ;; https://github.com/tarsius/keycast/issues/7#issuecomment-627604064
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (add-hook 'pre-command-hook 'keycast--update t)
      (remove-hook 'pre-command-hook 'keycast--update)))

  (add-to-list 'global-mode-string '("" mode-line-keycast " ")))

;; 统计各个命令的使用次数
;; M-x keyfreq-show
;; - https://github.com/dacap/keyfreq
(use-package keyfreq
  :disabled t
  :commands (keyfreq-show)
  :init
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(provide 'init-which-key)
;;; init-which-key.el ends here
