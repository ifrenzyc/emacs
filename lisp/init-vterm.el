;; init-vterm.el --- Initialize vterm configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Need to have libvterm installed in your system.
;; 
;; On macOS:
;; brew install libvterm

;;; Code:

(use-package vterm
  :hook
  (;; 为 vterm 设置单独字体，https://emacs-china.org/t/mode/15512
   (vterm-mode . (lambda () (setq buffer-face-mode-face '((:family "Cascadia Code" :height 130)))
                   (buffer-face-mode t))))
  :custom ((vterm-buffer-name-string "vterminal %s")
           (vterm-kill-buffer-on-exit t)
           (vterm-max-scrollback 100000))
  :init
  ;; kill vterm buffers when exiting with C-d
  (defun my/vterm-exit-kill-buffer (buffer event)
    (kill-buffer buffer)
    (delete-window))
  (setq vterm-exit-functions '(my/vterm-exit-kill-buffer)))

(use-package vterm-toggle
  ;; :straight (:host github :repo "jixiuf/vterm-toggle")
  :after vterm
  :commands (vterm-toggle vterm-toggle-cd vterm)
  :config
  ;; show vterm buffer in bottom side
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
               '("^v?term.*"
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 ;;(display-buffer-reuse-window display-buffer-in-direction)
                 ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                 ;;(direction . bottom)
                 ;;(dedicated . t) ;dedicated is supported in emacs27
                 (reusable-frames . visible)
                 (window-height . 0.3))))

;; https://github.com/suonlight/multi-libvterm
(use-package multi-vterm
  :after vterm
  :commands (multi-vterm multi-vterm-project)
  :general
  (vterm-mode-map
   ;; Switch to next/previous vterm buffer
   "s-n"   'multi-vterm-next
   "s-p"   'multi-vterm-prev
   "<f8>"  'treemacs
   "<f9>"  'shell-pop)
  ("<f2>"  'multi-vterm)
  :config
  (defalias 'mt 'multi-vterm-project))

(provide 'init-vterm)
