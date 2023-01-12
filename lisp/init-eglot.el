;; init-eglot.el --- Initialize eglot configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:
(use-package eglot
  :pin melpa
  :hook ((eglot--managed-mode . (lambda ()
                                  (flymake-mode -1)))
         ((java-mode java-ts-mode c-mode c-ts-mode c++-mode c++-ts-mode python-mode python-ts-mode html-mode css-mode clojure-mode dart-mode) . eglot-ensure))
  :custom
  (eglot-ignored-server-capabilites '(:documentHighlightProvider
                                      :signatureHelpProvider
                                      :hoverProvider))
  :config
  (progn
    (setenv "CLASSPATH"
            (concat (getenv "CLASSPATH") ":" (expand-file-name ".cache/lsp/eclipse.jdt.ls/plugins/org.eclipse.equinox.launcher_1.6.400.v20210924-0641.jar" user-emacs-directory)))
    (mapc (lambda (pair)
            (add-to-list 'eglot-server-programs pair))
          '((clojure-mode . ("bash" "-c" "clojure-lsp"))
            (html-mode . ("html-languageserver" "--stdio"))
            (css-mode . ("css-languageserver" "--stdio"))
            (js-mode . ("javascript-typescript-stdio"))
            (c++-mode . ("clangd"))
            (c-mode . ("clangd"))
            (rust-mode . ("rls"))))))

(provide 'init-eglot)
