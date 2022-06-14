;; lang-ansible.el --- Initialize ansible settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code

(use-package ansible)
(use-package ansible-doc
  :config
  (add-hook 'yaml-mode-hook #'ansible-mode)
  (add-hook 'yaml-mode-hook #'ansible-doc-mode)
  )
;; (use-package ansible-vault
;;   :init (add-hook 'yaml-mode-hook 'ansible-vault-mode))
(use-package company-ansible
  :config
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-ansible)))

(use-package poly-ansible)

(provide 'lang-ansible)
