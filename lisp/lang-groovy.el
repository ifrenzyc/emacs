;; lang-groovy.el --- Initialize groovy settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; brew install groovy

;;; Code:
(use-package groovy-mode
  :ensure-system-package (groovy . "brew install groovy")
  :custom
  (groovy-indent-offset 4)
  (lsp-groovy-server-file "~/src/opensource/groovy-language-server/build/libs/groovy-language-server-all.jar")
  (lsp-groovy-classpath "/opt/homebrew/opt/groovy/libexec/lib")
  :mode (("\\.groovy$" . groovy-mode)
         ("\\.gradle$" . groovy-mode))
  :hook
  (groovy-mode . lsp))

(provide 'lang-groovy)
;;; lang-groovy.el ends here
