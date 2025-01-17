;; lang-docker.el --- Initialize docker settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:

;;================================================================================
(use-package docker
  :disabled t
  :bind
  ("C-c d" . docker))
;;================================================================================

(provide 'lang-docker)
;;; lang-docker.el ends here
