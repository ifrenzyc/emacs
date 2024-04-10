;; init-reader.el --- Initialize reader configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; Darkroom vs Olivetti vs Writeroom-mode
;; 

;;; Code:

(use-package olivetti
  :commands (olivetti-mode)
  :hook
  (olivetti . diff-hl-mode)
  :config
  (setq olivetti-body-width 89
        olivetti-hide-mode-line t))

;; add view mode keybindings
;; - https://gist.github.com/ivan-krukov/63a586f2121519ca51b201c634402a84
(use-package view
  :disabled t
  :ensure nil
  :bind (("<f18>" . view-mode)  ;; remap R-Shift to F 13
         :map view-mode-map
         ("n" . next-line)
         ("p" . previous-line))
  :config
  ;; make sure the cursor is changed visually
  ;; (set-cursor-color "DarkCyan")
  ;; (setq-default cursor-type 'box)

  ;; (defun xfk-command-mode-on ()
  ;;   (global-hl-line-mode 1)
  ;;   (set-cursor-color "yellow")
  ;;   (setq cursor-type 'box))
  ;; (defun xfk-insert-mode-on ()
  ;;   (global-hl-line-mode 0)
  ;;   (set-cursor-color "DarkCyan")
  ;;   (setq cursor-type 'box))
  ;; :hook
  ;; (view-mode . (defun view-mode-hookee+ ()
  ;;                (if view-mode
  ;;                    (xfk-command-mode-on)
  ;;                  (xfk-insert-mode-on))))
  :hook
  ;; (view-mode . (defun view-mode-hookee+ ()
  ;;                (setq cursor-type (if view-mode 'box 'bar))))
  (view-mode . (lambda ()
                 (setq cursor-type (if view-mode 'hollow 'box)))))

(use-package writeroom-mode
  :disabled t
  :commands (writeroom-mode)
  :bind
  (:map writeroom-mode-map
        ("C-M-<" . writeroom-decrease-width)
        ("C-M->" . writeroom-increase-width)
        ("C-M-=" . writeroom-adjust-width))
  :config
  (setq writeroom-restore-window-config t
        writeroom-width 89))

(provide 'init-reader)
;;; init-reader.el ends here
