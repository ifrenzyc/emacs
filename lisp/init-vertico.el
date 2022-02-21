;; init-vertico.el --- Initialize vertico settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code

(use-package orderless
  :init (vertico-mode) ; optional but recommended!
  :custom (completion-styles '(orderless)))


(provide 'init-vertico)
