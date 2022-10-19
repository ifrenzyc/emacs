;; lang-ansible.el --- Initialize ansible settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:

(use-package ansible
  :hook
  (yaml-mode . ansible-mode))

(use-package ansible-doc
  :hook
  (yaml-mode . ansible-doc-mode))

;; (use-package ansible-vault
;;   :init (add-hook 'yaml-mode-hook 'ansible-vault-mode))

(use-package company-ansible
  :config
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-ansible)))

(use-package poly-ansible)

(provide 'lang-ansible)
