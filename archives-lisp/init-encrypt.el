;; init-encrypt.el --- Initialize encrypt settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; $ brew install gnupg
;; 

;;; Code:

(require 'epa-file)
(custom-set-variables '(epg-gpg-program  "/usr/local/bin/gpg"))
(epa-file-enable)

(provide 'init-encrypt)
;;; init-encrypt.el ends here
