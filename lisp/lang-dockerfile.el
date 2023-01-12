;; lang-dockerfile.el --- Initialize dockerfile settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-ts-mode)
  :hook
  ((dockerfile-mode . lsp-deferred)
   (dockerfile-ts-mode . lsp-deferred))
  :general
  (dockerfile-ts-mode-map
   "C-c C-f" 'lsp-format-buffer))

(use-package docker-compose-mode
  :config
  (add-to-list 'auto-mode-alist '("docker-compose[^/]*\\.yml\\'" . docker-compose-mode)))

(provide 'lang-dockerfile)
