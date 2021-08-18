;; init-meow.el --- Initialize meow settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; - https://github.com/DogLooksGood/meow
;; or objed: https://github.com/clemera/objed
;; 

;;; Code

(use-package meow
  :demand t
  :init
  (meow-global-mode 1)
  :custom
  ;; layout options: qwerty, dvorak, dvp, colemak
  (meow-layout 'qwerty)
  :config
  (meow-leader-define-key
   '("k" . kill-buffer)
   '("l" . goto-line)
   '("h" . other-window)
   '("o" . delete-other-windows)
   '("-" . split-window-below)
   '("/" . swiper)
   '("\\" . split-window-right)
   '("m" . magit-status)
   '("f" . find-file)
   '("F" . find-file-literally))
  (meow-leader-define-mode-key
   'emacs-lisp-mode
   '("RET" . eval-buffer)
   '("SPC" . eval-defun)))

(provide 'init-meow)
