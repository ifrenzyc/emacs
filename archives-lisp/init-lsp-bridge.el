;; init-lsp-bridge.el --- Initialize lsp-bridge settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; - https://github.com/emacs-lsp/lsp-mode
;; 
;; 

;;; Code
;; 融合 `lsp-bridge' `find-function' 以及 `dumb-jump' 的智能跳转
(defun lsp-bridge-jump ()
  (interactive)
  (cond
    ((eq major-mode 'emacs-lisp-mode)
     (let ((symb (function-called-at-point)))
       (when symb
         (find-function symb))))
    (lsp-bridge-mode
     (lsp-bridge-find-def))
    (t
     (require 'dumb-jump)
     (dumb-jump-go))))

(defun lsp-bridge-jump-back ()
  (interactive)
  (cond
    (lsp-bridge-mode
     (lsp-bridge-return-from-def))
    (t
     (require 'dumb-jump)
     (dumb-jump-back))))

(use-package lsp-bridge
  :demand t
  :after (markdown-mode)
  :load-path "localelpa/lsp-bridge"
  ;; :straight (:host github :repo "manateelazycat/lsp-bridge" :files ("*" "acm/*"))
  :bind
  (:map lsp-bridge-mode-map
        ("M-."   . lsp-bridge-jump)
        ("M-,"   . lsp-bridge-jump-back)
        ("M-?"   . lsp-bridge-find-references)
        ("M-i"   . lsp-bridge-lookup-documentation)
        ("M-n"   . lsp-bridge-popup-documentation-scroll-up)
        ("M-p"   . lsp-bridge-popup-documentation-scroll-down)
        ("s-C-n" . lsp-bridge-jump-to-next-diagnostic)
        ("s-C-p" . lsp-bridge-jump-to-prev-diagnostic))
  :config
  (setq lsp-bridge-enable-log t)
  (setq lsp-bridge-enable-signature-help t)
  ;; (require 'lsp-bridge-icon)
  (require 'lsp-bridge-epc)
  (require 'lsp-bridge-jdtls)
  ;; (require 'lsp-bridge-orderless)
  (global-lsp-bridge-mode)
  (add-to-list 'lsp-bridge-org-babel-lang-list "emacs-lisp")
  (add-to-list 'lsp-bridge-org-babel-lang-list "sh")
  (add-to-list 'lsp-bridge-org-babel-lang-list "shell")
  (when (> (frame-pixel-width) 3000) (custom-set-faces '(corfu-default ((t (:height 1.3))))))
  ;; ;; 通过Cape融合不同的补全后端，比如lsp-bridge、 tabnine、 file、 dabbrev.
  ;; (defun lsp-bridge-mix-multi-backends ()
  ;;   (setq-local completion-category-defaults nil)
  ;;   (setq-local completion-at-point-functions
  ;;               (list
  ;;                (cape-capf-buster
  ;;                 (cape-super-capf
  ;;                  #'lsp-bridge-capf
  ;;                  #'cape-file
  ;;                  #'cape-dabbrev
  ;;                  )
  ;;                 'equal)
  ;;                )))

  ;; (dolist (hook lsp-bridge-default-mode-hooks)
  ;;   (add-hook hook (lambda ()
  ;;                    (lsp-bridge-mix-multi-backends) ; 通过 Cape 融合多个补全后端
  ;;                    )))
  )

(provide 'init-lsp-bridge)
