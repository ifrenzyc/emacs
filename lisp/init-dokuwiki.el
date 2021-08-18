;; init-dokuwiki.el --- Initialize dokuwiki settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code

(use-package dokuwiki-mode
  :config
  (use-package outline-magic))

(use-package dokuwiki
  :init
  (use-package xml-rpc))


(provide 'init-dokuwiki)
