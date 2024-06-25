;; init-xah-fly-keys.el --- Initialize xah-fly-keys settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:

(use-package xah-fly-keys
  :init
  (setq xah-fly-use-control-key nil)
  (defun xfk-command-mode-on ()
    (global-hl-line-mode 1)
    (set-cursor-color "deep pink")
    (setq cursor-type 'box)
    )
  (defun xfk-insert-mode-on ()
    (global-hl-line-mode 0)
    (set-cursor-color "Dark Turquoise")
    (setq cursor-type 'hbar)
    )
  :config
  ;; comment out for default dvorak layout
  (xah-fly-keys-set-layout "qwerty") ; required if you use qwerty
  (message "activating xah-fly-keys")
  (xah-fly-keys 1)
  (add-hook 'xah-fly-command-mode-activate-hook 'xfk-command-mode-on)
  (add-hook 'xah-fly-insert-mode-activate-hook  'xfk-insert-mode-on)
  (add-hook 'magit-mode-hook 'xah-fly-insert-mode-activate)
  (add-hook 'magit-popup-mode-hook 'xah-fly-insert-mode-activate))

(provide 'init-xah-fly-keys)
;;; init-xah-fly-keys.el ends here
