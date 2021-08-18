;; init-icomplete.el --- Initialize icomplete settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; selectrum + prescient
;; icomplete
;; icomplete-vertical
;; 

;;; Code

(use-package orderless
  :init (icomplete-mode) ; optional but recommended!
  :custom (completion-styles '(orderless)))

;; - https://github.com/oantolin/icomplete-vertical
(use-package icomplete-vertical
  :demand t
  :custom
  (completion-styles '(partial-completion substring))
  (completion-category-overrides '((file (styles basic substring))))
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  :config
  (icomplete-mode)
  (icomplete-vertical-mode)
  :bind (:map icomplete-minibuffer-map
              ("<down>" . icomplete-forward-completions)
              ("C-n" . icomplete-forward-completions)
              ("<up>" . icomplete-backward-completions)
              ("C-p" . icomplete-backward-completions)
              ("C-v" . icomplete-vertical-toggle)))


(provide 'init-icomplete)
