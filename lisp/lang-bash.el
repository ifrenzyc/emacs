;; lang-bash.el --- Initialize bash settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:
(use-package shell-mode
  :ensure nil
  :hook
  ((sh-mode . lsp)
   (bash-ts-mode . lsp)
   (after-save-hook . executable-make-buffer-file-executable-if-script-p)))

(use-package company-shell
  :after (company shell-mode)
  :hook
  (sh-mode . company-mode)
  :config
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-shell)))

(use-package fish-mode
  :mode ("\\.fish\\'" . fish-mode))

(use-package flycheck-checkbashisms
  :ensure-system-package (checkbashisms . "brew install checkbashisms")
  :custom
  ;; Check 'echo -n' usage
  (flycheck-checkbashisms-newline t)

  ;; Check non-POSIX issues but required to be supported  by Debian Policy 10.4
  ;; Setting this variable to non nil made flycheck-checkbashisms-newline effects
  ;; regardless of its value
  (flycheck-checkbashisms-posix t)
  :config
  (flycheck-checkbashisms-setup))

(use-package shfmt
  :bind
  (:map sh-mode-map
        ("C-c C-f" . shfmt)))

(provide 'lang-bash)
;;; lang-bash.el ends here
