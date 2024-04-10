;; lang-json.el --- Initialize json settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:
(use-package jsonian
  :mode ("\\.\\(json\\|json.j2\\|json.template\\|jsonld\\|tern-project\\|jshintrc\\)$" . jsonian-mode)
  :general
  (jsonian-mode-map
   "C-c C-f" 'json-mode-beautify)
  :custom
  (jsonian-no-so-long-mode)
  :hook
  (jsonian-mode . jsonian-enable-flycheck)
  (jsonian-mode . lsp-deferred))

(use-package json-mode
  :disabled t
  :general
  (json-ts-mode-map
   "C-c C-f" 'json-mode-beautify)
  :mode ("\\.\\(json\\|json.j2\\|json.template\\|jsonld\\|tern-project\\|jshintrc\\)$" . json-ts-mode)
  :hook
  (json-ts-mode . flycheck-mode)
  (json-ts-mode . lsp-deferred))

(provide 'lang-json)
;;; lang-json.el ends here
