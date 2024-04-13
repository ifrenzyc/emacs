;; lang-dockerfile.el --- Initialize dockerfile settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-ts-mode)
  :hook
  ((dockerfile-mode . lsp-deferred)
   (dockerfile-ts-mode . lsp-deferred))
  :bind
  (:map dockerfile-mode-map
        ("C-c C-f" . lsp-format-buffer)))

(use-package docker-compose-mode
  :mode ("docker-compose[^/]*\\.\\(yml\\|yaml\\)$" . docker-compose-mode))

(provide 'lang-dockerfile)
;;; lang-dockerfile.el ends here
