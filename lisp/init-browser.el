;; init-browser.el --- Initialize browser settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; - https://github.com/akirakyle/emacs-webkit
;; 

;;; Code:

;; (set browse-url-browser-function 'xwidget-webkit-browse-url)

;; xwidget-webkit enhancement suite
;; - https://github.com/BlueFlo0d/xwwp
(use-package xwwp-follow-link
  :ensure nil
  :load-path "localelpa/xwwp"
  :bind
  (:map xwidget-webkit-mode-map
        ("v" . xwwp-follow-link))
  :custom
  (xwwp-follow-link-completion-system 'ivy))

(provide 'init-browser)
;;; init-browser.el ends here
