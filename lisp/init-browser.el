;; init-browser.el --- Initialize browser settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; - https://github.com/akirakyle/emacs-webkit
;; 

;;; Code

;; (set browse-url-browser-function 'xwidget-webkit-browse-url)

;; xwidget-webkit enhancement suite
;; - https://github.com/BlueFlo0d/xwwp
(use-package xwwp-follow-link
  :ensure nil
  :load-path "localelpa/xwwp"
  :custom
  (xwwp-follow-link-completion-system 'ivy)
  :general (xwidget-webkit-mode-map
            "v" 'xwwp-follow-link))

(provide 'init-browser)
