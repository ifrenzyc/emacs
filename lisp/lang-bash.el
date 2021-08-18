;; lang-bash.el --- Initialize bash settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(use-package fish-mode
  :mode ("\\.fish\\'" . fish-mode))

(use-package flycheck-checkbashisms
  :ensure-system-package (checkbashisms . "brew install checkbashisms")
  :config
  ;; Check 'echo -n' usage
  (setq flycheck-checkbashisms-newline t)

  ;; Check non-POSIX issues but required to be supported  by Debian Policy 10.4
  ;; Setting this variable to non nil made flycheck-checkbashisms-newline effects
  ;; regardless of its value
  (setq flycheck-checkbashisms-posix t)
  (flycheck-checkbashisms-setup))

(use-package shfmt
  :general
  (sh-mode-map
   "C-c C-f" 'shfmt))

(provide 'lang-bash)
