;; lang-python.el --- Initialize python settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; - Python 开发环境设置：http://wikemacs.org/wiki/Python#other_ELPA_packages
;; 

;;; Code:
(require 'init-const)
(require 'init-hydra)

(use-package python
  :mode ("\\.py\\'" . python-ts-mode)
  :mode-hydra
  (python-ts-mode
   (:title "Python Commands")
   ("Python"
    (("i" elpy-importmagic-fixup "Importmagic fixup")
     ("d" elpy-goto-definition   "Goto definition")
     ("r" elpy-multiedit-python-symbol-at-point   "Rename symbol")
     ("f" elpy-format-code   "Format code"))))
  :hook
  (python-ts-mode . diff-hl-mode)
  :custom
  ;; use the wx backend, for both mayavi and matplotlib
  (py-python-command-args '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
  (py-force-py-shell-name-p t)

  ;; switch to the interpreter after executing code
  (py-shell-switch-buffers-on-execute-p t)
  (py-switch-buffers-on-execute-p t)
  ;; don't split windows
  (py-split-windows-on-execute-p nil)
  ;; try to automagically figure out indentation
  (py-smart-indentation t)
  :config
  ;; use IPython
  (setq-default py-shell-name "ipython")
  (setq-default py-which-bufname "IPython"))

(use-package lsp-pyright
  :hook
  (python-ts-mode . (lambda ()
                      (require 'lsp-pyright)
                      (lsp))))

(use-package anaconda-mode
  :after python
  :init
  (setq anaconda-mode-installation-directory
        (concat yc/cache-dir "/anaconda-mode"))
  :hook
  (python-ts-mode . anaconda-mode)
  (python-ts-mode . anaconda-eldoc-mode))

(use-package company-anaconda
  :after (anaconda python)
  :config
  (with-eval-after-load 'company
    (add-to-list 'company-backends '(company-anaconda :with company-capf))))

(use-package elpy
  :after python
  :config
  (elpy-enable))

(use-package pyenv-mode
  :after python
  :if (executable-find "pyenv")
  :commands (pyenv-mode-versions))

(use-package pyvenv
  :after python)

(use-package py-isort
  :after python)

(use-package yapfify
  :ensure-system-package (yapf . "brew install yapf")
  :after python
  ;; :hook (python-mode . yapf-mode)
  )

(use-package live-py-mode
  :after python)

(provide 'lang-python)
;;; lang-python.el ends here
