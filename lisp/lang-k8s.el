;; lang-k8s.el --- Initialize k8s settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; kubernetes
;; - https://www.reddit.com/r/emacs/comments/ci7s53/conquering_kubernetes_with_emacs/
;; 

;;; Code:
(use-package k8s-mode
  :config
  (setq k8s-search-documentation-browser-function 'browse-url-chrome)
  :hook (k8s-mode . yas-minor-mode))

(use-package kubernetes
  :commands (kubernetes-overview))

;; https://github.com/abrochard/kubel
;; Emacs extension for controlling Kubernetes with limited permissions.
(use-package kubel
  :after (vterm)
  :config (kubel-vterm-setup))

(provide 'lang-k8s)
;;; lang-k8s.el ends here
