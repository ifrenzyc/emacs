;; lang-restclient.el --- Initialize restclient settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; - https://github.com/pashky/restclient.el
;; 

;;; Code:
(use-package restclient
  :commands (restclient-mode)
  :mode ("\\.\\(http\\|rest\\)$" . restclient-mode))

(use-package ob-restclient
  :after (org restclient)
  :config
  (add-to-list 'org-babel-load-languages '(restclient . t)))

(use-package ob-http
  :commands org-babel-execute:http)

(use-package company-restclient
  :after (restclient)
  :config
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-restclient)))

(provide 'lang-restclient)
;;; lang-restclient.el ends here
