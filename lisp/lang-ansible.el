;; lang-ansible.el --- Initialize ansible settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:
(use-package poly-ansible)

;;================================================================================
(use-package ansible
  :disabled t
  :hook
  (yaml-mode . ansible-mode))

(use-package ansible-doc
  :disabled t
  :hook
  (yaml-mode . ansible-doc-mode))

;; (use-package ansible-vault
;;   :init (add-hook 'yaml-mode-hook 'ansible-vault-mode))

(use-package company-ansible
  :disabled t
  :config
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-ansible)))
;;================================================================================

(provide 'lang-ansible)
;;; lang-ansible.el ends here
