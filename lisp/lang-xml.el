;; lang-xml.el --- Initialize xml-mode settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code

(use-package nxml-mode
  :straight (:type built-in)
  :mode ("\\.\\(xml\\|gapp\\|plist\\|pom\\|xsd\\|xslt\\)$" . nxml-mode)
  :hook
  (nxml-mode . lsp-deferred)
  ;; (nxml-mode . highlight-indent-guides-mode)
  :config
  (setq-default nxml-child-indent 4
                nxml-attribute-indent 4)
  (setq nxml-slash-auto-complete-flag t
        nxml-auto-insert-xml-declaration-flag t))

(use-package xml-format
  :after nxml-mode
  :ensure-system-package (xmllint . "brew install libxml2")
  :general
  (nxml-mode-map
   "C-c C-f" 'xml-format))

(use-package noxml-fold
  :hook (nxml-mode . noxml-fold-mode))

(provide 'lang-xml)
