;; init-embark.el --- Initialize embark settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:
(require 'init-which-key)

(use-package embark
  :bind
  (("s-o" . embark-act)
   ([remap describe-bindings] . embark-bindings)
   :map minibuffer-local-map
   ("C-c C-c" . embark-export)
   ("C-c C-o" . embark-collect))
  :config
  ;; Use Embark instead of `describe-prefix-bindings'
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; From the embark wiki
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (caar targets) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t))))

  ;; Embark indicators
  (setq embark-indicators '(embark-which-key-indicator
                            embark-highlight-indicator
                            embark-isearch-highlight-indicator))

  (setq embark-verbose-indicator-display-action
        '(display-buffer-at-bottom
          (window-height . (lambda (win) (fit-window-to-buffer
                                          win (floor (frame-height) 
                                                     3))))))

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after embark consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'init-embark)
;;; init-embark.el ends here
