;; init-embark.el --- Initialize embark settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code

(use-package embark
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
                            ;; embark-minimal-indicator
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
                 (window-parameters (mode-line-format . none))))
  :general
  ("s-o" 'embark-act))

(provide 'init-embark)
