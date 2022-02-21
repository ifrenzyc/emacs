;; lang-docker.el --- Initialize docker settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code

(use-package docker
  :bind ("C-c d" . docker))

(provide 'lang-docker)
