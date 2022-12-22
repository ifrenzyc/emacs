;; lang-yaml.el --- Initialize yaml settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code

(use-package yaml-mode
  :mode ("\\.\\(yml\\|yaml\\|yml.j2\\|yaml.j2\\|yml.template\\|yaml.template\\)$" . yaml-mode)
  :hook
  (yaml-mode . lsp-deferred)
  :general
  (yaml-mode-map
   "C-c C-f" 'lsp-format-buffer))

(provide 'lang-yaml)
