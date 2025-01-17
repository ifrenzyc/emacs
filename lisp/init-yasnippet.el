;; init-yasnippet.el --- Initialize yasnippet settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;;

;;; Code:
(require 'init-funcs)

(use-package yasnippet
  :after org
  :hook
  (prog-mode . yas-minor-mode)
  (org-mode . yas-minor-mode)
  :custom
  ;; Suppress excessive log messages
  (yas-verbosity 1)
  (yas-triggers-in-field t)
  (yas-prompt-functions '(yas-dropdown-prompt yas-completing-prompt yas-maybe-ido-prompt yas-no-prompt))
  :config
  (yas-global-mode t))

;;================================================================================
(use-package yasnippet-snippets
  :disabled t
  :after yasnippet)
;;================================================================================

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
