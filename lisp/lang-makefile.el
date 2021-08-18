;; lang-makefile.el --- Initialize makefile settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code

(use-package make-mode
  :straight (:type built-in)
  :mode (("Makefile" . makefile-gmake-mode)))

(provide 'lang-makefile)
