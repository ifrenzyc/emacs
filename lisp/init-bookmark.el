;; init-bookmark.el --- Initialize bookmark settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code

(use-package bookmark
  :ensure nil
  :config
  ;; disable bookmark face after org-capture
  (setq bookmark-fontify nil))

(provide 'init-bookmark)
