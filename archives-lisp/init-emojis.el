;; init-emojis.el --- Initialize emojis settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; Emojis in Emacs! :cat:
;; - https://github.com/iqbalansari/emacs-emojify
;; - https://www.youtube.com/watch?v=aeH2Z-nzYTs
;; 

;;; Code

(use-package emojify
  :init
  (global-emojify-mode 1)
  :hook
  (after-init . global-emojify-mode)
  :config
  (if (display-graphic-p)
      (setq emojify-display-style 'image)
    (setq emojify-display-style 'unicode))
  (setq emojify-emoji-set "emojione-v2.2.6")
  (when (memq window-system '(mac ns))
    (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))

  (defun yc/set-emoji-font (frame)
    "Adjust the font settings of FRAME so Emacs can display emoji properly."
    (if (eq system-type 'darwin)
        ;; For NS/Cocoa
        (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
      ;; For Linux
      (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend)))

  ;; For when Emacs is started in GUI mode:
  (yc/set-emoji-font nil)
  ;; Hook for when a frame is created with emacsclient
  ;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
  (add-hook 'after-make-frame-functions 'yc/set-emoji-font))

(use-package company-emoji
  :config
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-emoji)))

(provide 'init-emojis)
