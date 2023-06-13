;; init-pandoc.el --- Initialize pandoc settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:

(use-package pandoc-mode
  :ensure-system-package (pandoc . "brew install pandoc")
  :hook
  (pandoc-mode . pandoc-load-default-settings)
  :config
  (when IS-MAC
    (add-to-list 'exec-path "/usr/local/texlive/2023/bin/universal-darwin/")))

(provide 'init-pandoc)
;;; init-pandoc.el ends here
