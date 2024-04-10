;; lang-yaml.el --- Initialize yaml settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:
(use-package yaml-mode
  :mode ("\\.\\(yml\\|yaml\\|yml.j2\\|yaml.j2\\|yml.template\\|yaml.template\\)$" . yaml-ts-mode)
  :general
  (yaml-ts-mode-map
   "C-c C-f" 'lsp-format-buffer)
  :hook
  (yaml-ts-mode . lsp-deferred))

(provide 'lang-yaml)
;;; lang-yaml.el ends here
