;; init-eglot.el --- Initialize eglot configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:
(use-package eglot
  :pin melpa
  :hook (((java-mode c-mode c++-mode python-mode html-mode css-mode clojure-mode dart-mode) . eglot-ensure))
  :custom
  (eglot-ignored-server-capabilites '(:documentHighlightProvider
                                      :signatureHelpProvider
                                      :hoverProvider))
  :config
  (add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))
  (progn
    (setenv "CLASSPATH"
            (concat (getenv "CLASSPATH") ":" (expand-file-name ".cache/lsp/eclipse.jdt.ls/plugins/org.eclipse.equinox.launcher_1.6.0.v20200915-1508.jar" user-emacs-directory)))
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
