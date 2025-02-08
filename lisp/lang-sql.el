;; lang-sql.el --- Initialize sql settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:
(use-package sql
  :ensure nil
  :mode ("\\.sql\\'" . sql-mode)
  :hook (sql-mode   . lsp))

(use-package sqlformat
  :ensure-system-package (sqlformat . "brew install sqlparse")
  :bind
  (:map sql-mode-map
        ("C-c C-f" . sqlformat)))

(use-package sqlup-mode
  :hook
  (sql-mode . sqlup-mode)
  :config
  (add-to-list 'sqlup-blacklist "name"))

(use-package sql-indent
  :hook
  (sql-mode . sqlind-minor-mode))

;; - https://github.com/kostafey/ejc-sql#use-with-org-mode
;; - https://gist.githubusercontent.com/stardiviner/7243e6c4a6ad4843304e796149f32c04/raw/d1eeda552b7a344a78d2e06330c78a357c32ecc6/ejc-sql%2520reproduce.org
(use-package ejc-sql
  :hook
  (sql-mode . ejc-sql-mode)
  (ejc-sql-minor-mode . (lambda ()
                          (company-mode t)
                          (ejc-eldoc-setup)))
  :custom
  (ejc-org-mode-show-results nil)
  :config
  (require 'ejc-company)
  ;; (add-to-list 'company-backends 'ejc-company-backend)
  )

(provide 'lang-sql)
;;; lang-sql.el ends here
