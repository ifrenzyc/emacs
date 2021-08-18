;; init-nox.el --- Initialize nox configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:

(use-package nox
  ;; :load-path "localelpa/nox"
  :straight (:host github
                   :repo "manateelazycat/nox"
                   :files ("jsonrpc.el" "nox.el"))
  ;; :init
  ;; (setq lsp-java-server-install-dir (expand-file-name "eclipse.jdt.ls/server/" user-emacs-directory)
  ;;       lsp-java-workspace-dir (expand-file-name "eclipse.jdt.ls/workspace/" user-emacs-directory))
  :config
  ;; (require 'nox)

  (dolist (hook (list
                 'js-mode-hook
                 'rust-mode-hook
                 'python-mode-hook
                 'ruby-mode-hook
                 'java-mode-hook
                 'sh-mode-hook
                 'php-mode-hook
                 'c-mode-common-hook
                 'c-mode-hook
                 'c++-mode-hook
                 'haskell-mode-hook))
    (add-hook hook '(lambda () (nox-ensure)))))

(provide 'init-nox)
