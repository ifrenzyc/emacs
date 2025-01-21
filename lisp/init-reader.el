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
  :custom
  ((olivetti-body-width 89)
   (olivetti-hide-mode-line t)))

;; add view mode keybindings
;; - https://gist.github.com/ivan-krukov/63a586f2121519ca51b201c634402a84
(use-package view
  :ensure nil
  :bind
  (:map view-mode-map
        ("n" . next-line)
        ("p" . previous-line)
        ("j" . next-line)
        ("k" . previous-line)
        ("J" . scroll-up-one-line)
        ("K" . scroll-down-one-line)
        ("g" . beginning-of-buffer)
        ("G" . end-of-buffer)
        ("h" . backward-char)
        ("l" . forward-char)
        ("H" . backward-word)
        ("L" . forward-word)
        ("<SPC>"   . scroll-up)
        ("S-<SPC>" . scroll-down)
        ("s" . color-rg-search-symbol)
        ("i" . color-rg-search-input)
        ("," . color-rg-search-symbol-in-current-file)
        ("." . color-rg-search-input-in-current-file)
        ("<" . remember-init)
        (">" . remember-jump))
  ;; :config
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
  :hook
  ;; (view-mode . (defun view-mode-hookee+ ()
  ;;                (if view-mode
  ;;                    (xfk-command-mode-on)
  ;;                  (xfk-insert-mode-on))))
  (view-mode . (lambda ()
                 (setq cursor-type (if view-mode 'hollow 'box)))))

;;================================================================================
(use-package writeroom-mode
  :disabled t
  :commands (writeroom-mode)
  :bind
  (:map writeroom-mode-map
        ("C-M-<" . writeroom-decrease-width)
        ("C-M->" . writeroom-increase-width)
        ("C-M-=" . writeroom-adjust-width))
  :custom
  ((writeroom-restore-window-config t)
   (writeroom-width 89)))
;;================================================================================

(provide 'init-reader)
;;; init-reader.el ends here
