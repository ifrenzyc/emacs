;; init-yasnippet.el --- Initialize yasnippet settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;;

;;; Code:
(require 'init-funcs)

(use-package yasnippet
  :hook
  (prog-mode . yas-minor-mode)
  (org-mode . yas-minor-mode)
  :config
  ;; Suppress excessive log messages
  (setq yas-verbosity 1
        yas-triggers-in-field t
        yas-prompt-functions '(yas-ido-prompt))
  (yas-global-mode t))

(use-package yasnippet-snippets
  :after yasnippet)

(provide 'init-yasnippet)
