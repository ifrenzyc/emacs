;; init-vterm.el --- Initialize vterm configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Need to have libvterm installed in your system.
;; - https://github.com/akermu/emacs-libvterm
;; 
;; On macOS:
;; brew install libvterm

;;; Code:
(use-package vterm
  :hook
  (;; 为 vterm 设置单独字体，https://emacs-china.org/t/mode/15512
   (vterm-mode . (lambda () (setq buffer-face-mode-face '((:family "Cascadia Code" :height 130)))
                   (buffer-face-mode t))))
  :custom
  ((vterm-buffer-name-string "*vterm %s*")
   (vterm-kill-buffer-on-exit t)
   (vterm-max-scrollback 100000))
  :init
  (setq-default shell-file-name "/opt/homebrew/bin/zsh")
  (setq vterm-always-compile-module t)

  (add-hook 'vterm-exit-functions
            (lambda (_ _)
              (let* ((buffer (current-buffer))
                     (window (get-buffer-window buffer)))
                (when (not (one-window-p))
                  (delete-window window))
                (kill-buffer buffer))))

  (add-to-list 'display-buffer-alist
               '("^*vterm .*"
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 ;;(display-buffer-reuse-window display-buffer-in-direction)
                 ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                 ;;(direction . bottom)
                 ;;(dedicated . t) ;dedicated is supported in emacs27
                 (reusable-frames . visible)
                 (window-height . 0.3)))
  :general
  ("<f9>"  'projectile-run-vterm
   "S-<f9>" 'vterm)
  (vterm-mode-map
   "<f8>"    'treemacs
   "M-SPC"   'yc/turn-off-input-method
   "H-SPC"   'yc/turn-on-rime-input-method)
  :config
  (defun my/vterm-execute-current-line ()
    "Insert text of current line in vterm and execute."
    (interactive)
    (require 'vterm)
    (eval-when-compile (require 'subr-x))
    (let ((command (string-trim (buffer-substring
                                 (save-excursion
                                   (beginning-of-line)
                                   (point))
                                 (save-excursion
                                   (end-of-line)
                                   (point))))))
      (let ((buf (current-buffer)))
        (unless (get-buffer vterm-buffer-name)
          (vterm))
        (display-buffer vterm-buffer-name t)
        (switch-to-buffer-other-window vterm-buffer-name)
        (vterm--goto-line -1)
        (message command)
        (vterm-send-string command)
        (vterm-send-return)
        (switch-to-buffer-other-window buf)
        )))
  (defun yc/vterm-other-frame ()
    "Create a new frame with a new vterm."
    (interactive)
    (make-frame)
    (vterm "Untitled-Vterm"))
  )

(use-package vterm-toggle
  :disabled t
  :commands (vterm-toggle vterm-toggle-cd vterm)
  :config
  ;; show vterm buffer in bottom side
  (setq vterm-toggle-fullscreen-p nil))

;; https://github.com/suonlight/multi-libvterm
(use-package multi-vterm
  :disabled t
  :commands (multi-vterm multi-vterm-project)
  :general
  (vterm-mode-map
   ;; Switch to next/previous vterm buffer
   "s-n"   'multi-vterm-next
   "s-p"   'multi-vterm-prev
   "<f8>"  'treemacs)
  :config
  (defalias 'mt 'multi-vterm-project))

(provide 'init-vterm)
;;; init-vterm.el ends here
