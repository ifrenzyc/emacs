;; lang-makefile.el --- Initialize makefile settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:

(use-package make-mode
  :ensure nil
  :mode ("Makefile" . makefile-gmake-mode))

(provide 'lang-makefile)
;;; lang-makefile.el ends here
