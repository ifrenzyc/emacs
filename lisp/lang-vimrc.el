;; lang-vimrc.el --- Initialize vimrc configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:
(use-package vimrc-mode
  :mode (".vimrc\\'" . vimrc-mode)
  :config
  (add-to-list 'org-src-lang-modes '("viml" . vimrc)))

(provide 'lang-vimrc)
;;; lang-vimrc.el ends here
