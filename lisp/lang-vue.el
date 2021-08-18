;; lang-vue.el --- Initialize vue settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; npm install -g vls
;; 

;;; Code

(use-package vue-mode
  ;; :after lsp-mode
  :mode ("\\.vue\\'" . vue-mode)
  ;; :init
  ;; ;; If the user has installed `vue-mode' then, by appending this to
  ;; ;; `auto-mode-alist' rather than prepending it, its autoload will have
  ;; ;; priority over this one.
  ;; (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode) 'append)
  :config
  ;; 0, 1, or 2, representing (respectively) none, low, and high coloring
  (setq mmm-submode-decoration-level 3
        vue-html-extra-indent 2
        css-indent-offset 2
        ;; lsp-ui-flycheck-enable nil
        ))

(use-package lsp-vue
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
