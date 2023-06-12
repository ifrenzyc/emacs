;; init-nov.el --- Initialize nov settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :init
  (add-hook 'nov-mode-hook 'visual-line-mode)
  (add-hook 'nov-mode-hook 'visual-fill-column-mode)
  (add-hook 'nov-mode-hook (lambda ()
			                 (display-line-numbers-mode -1)))
  :config
  (setq-local visual-fill-column-center-text t)
  (nov-mode-map
   "H" 'nov-previous-document
   "L" 'nov-next-document
   "[" 'nov-previous-document
   "]" 'nov-next-document
   "d" 'nov-scroll-up
   "u" 'nov-scroll-down
   "m" 'nov-display-metadata
   "r" 'nov-render-document
   "t" 'nov-goto-toc
   "v" 'nov-view-source
   "V" 'nov-view-content-source
   "q" 'quit-window))

(provide 'init-nov)
