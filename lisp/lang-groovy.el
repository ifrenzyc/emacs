;; lang-groovy.el --- Initialize groovy settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; brew install groovy

;;; Code

(use-package groovy-mode
  :ensure-system-package (groovy . "brew install groovy")
  :custom
  (groovy-indent-offset 2)
  (lsp-groovy-server-file "/Users/yangc/src/opensource/groovy-language-server/build/libs/groovy-language-server-all.jar")
  (lsp-groovy-classpath "/usr/local/opt/groovy/libexec/lib")
  :hook (groovy-mode . lsp-deferred)
  :mode (("\\.groovy$" . groovy-mode)
         ("\\.gradle$" . groovy-mode)))

(provide 'lang-groovy)
