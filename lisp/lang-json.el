;; lang-json.el --- Initialize json settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:
(use-package json-mode
  :mode ("\\.\\(json\\|json.j2\\|json.template\\|jsonld\\|tern-project\\|jshintrc\\)$" . json-ts-mode)
  :hook
  (json-ts-mode . flycheck-mode)
  (json-ts-mode . lsp-deferred)
  :general
  (json-ts-mode-map
   "C-c C-f" 'json-mode-beautify))

(provide 'lang-json)
;;; lang-json.el ends here
