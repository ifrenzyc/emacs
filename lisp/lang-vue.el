;; lang-vue.el --- Initialize vue settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; npm install -g vls
;; 

;;; Code:
(use-package vue-mode
  :mode ("\\.vue\\'" . vue-mode)
  :custom-face
  (mmm-default-submode-face ((t (:background nil))))
  :custom
  ;; 0, 1, or 2, representing (respectively) none, low, and high coloring
  ((mmm-submode-decoration-level 3)
   (vue-html-extra-indent 2)
   (css-indent-offset 2)))

(use-package lsp-vue
  :ensure nil
  :after (vue-mode lsp-mode)
  :hook
  ((vue-mode . lsp)
   ;; (vue-mode . lsp-vue-mmm-enable)
   )
  ;; :config
  ;; (setq vetur.validation.template t)
  )

(use-package vue-html-mode)

(provide 'lang-vue)
;;; lang-vue.el ends here
