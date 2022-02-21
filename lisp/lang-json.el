;; lang-json.el --- Initialize json settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code

(use-package json-mode
  :mode ("\\.\\(json\\|json.j2\\|json.template\\|jsonld\\|tern-project\\|jshintrc\\)$" . json-mode)
  :hook
  (json-mode . flycheck-mode)
  (json-mode . lsp-deferred)
  :general
  (json-mode-map
   "C-c C-f" 'json-mode-beautify))

(provide 'lang-json)
