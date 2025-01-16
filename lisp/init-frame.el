;; init-frame.el --- Initialize frame settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:
(require 'init-funcs)

(use-package nameframe
  :disabled t
  ;; :bind
  ;; ("M-P" . nameframe-switch-frame)
  )

(use-package nameframe-projectile
  :disabled t
  :after projectile
  :config
  (nameframe-projectile-mode t))

(provide 'init-frame)
;;; init-frame.el ends here
