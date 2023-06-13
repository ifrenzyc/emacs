;; lang-systemd.el --- Initialize systemd settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:
(use-package systemd
  :mode ("\\.\\(service\\|service.j2\\|service.template\\)$" . systemd-mode))

(provide 'lang-systemd)
;;; lang-systemd.el ends here
