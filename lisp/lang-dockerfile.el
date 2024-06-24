;; lang-dockerfile.el --- Initialize dockerfile settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-ts-mode)
  :mode ("\\`Dockerfile\\.[a-zA-Z]+\\([^e]\\|e[^l]\\|el\\)[ \t]*\\'" . dockerfile-ts-mode)
  :hook
  ((dockerfile-mode    . lsp)
   (dockerfile-ts-mode . lsp)
   ((dockerfile-ts-mode dockerfile-mode) . (lambda ()
                                             (setq-local aggressive-indent-mode nil))))
  :bind
  (:map dockerfile-mode-map
        ("C-c C-f" . lsp-format-buffer))
  :custom
  (dockerfile-enable-auto-indent nil))

(use-package docker-compose-mode
  :mode ("docker-compose[^/]*\\.\\(yml\\|yaml\\)$" . docker-compose-mode))

(provide 'lang-dockerfile)
;;; lang-dockerfile.el ends here
