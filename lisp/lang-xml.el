;; lang-xml.el --- Initialize xml-mode settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:
(use-package nxml-mode
  :ensure nil
  :mode ("\\.\\(xml\\|gapp\\|plist\\|pom\\|xsd\\|xslt\\)$" . nxml-mode)
  :hook
  (nxml-mode . lsp-deferred)
  ;; (nxml-mode . highlight-indent-guides-mode)
  :custom
  ;; lsp-xml 依赖 eclipse/lemminx 项目，工程： ~/src/opensource/lemminx
  ;; 需要将 jar 包拷贝 emacs 下，并指定路径
  ;; 参考 https://emacs-lsp.github.io/lsp-mode/page/lsp-xml/
  (lsp-xml-jar-file (expand-file-name "bin/org.eclipse.lemminx-uber.jar" user-emacs-directory))
  :config
  (setq-default nxml-child-indent 4
                nxml-attribute-indent 4)
  (setq nxml-slash-auto-complete-flag t
        nxml-auto-insert-xml-declaration-flag t)
  :general
  (nxml-mode-map
   "C-c C-f" 'lsp-format-buffer))

(use-package xml-format
  :disabled t
  :after nxml-mode
  :ensure-system-package (xmllint . "brew install libxml2")
  :general
  (nxml-mode-map
   "C-c C-f" 'xml-format))

(use-package noxml-fold
  :disabled t
  :hook (nxml-mode . noxml-fold-mode))

(use-package hideshow
  :hook
  (nxml-mode . hs-minor-mode)
  :config
  (add-to-list 'hs-special-modes-alist
               '(nxml-mode
                 "<!--\\|<[^/>]*[^/]>"
                 "-->\\|</[^/>]*[^/]>"
                 "<!--"
                 nxml-forward-element
                 nil)))

(provide 'lang-xml)
;;; lang-xml.el ends here
