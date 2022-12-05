;; lang-python.el --- Initialize python settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; - Python 开发环境设置：http://wikemacs.org/wiki/Python#other_ELPA_packages
;; 

;;; Code:

(require 'init-const)

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :custom
  ;; use the wx backend, for both mayavi and matplotlib
  (py-python-command-args
   '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
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
  (setq-default py-which-bufname "IPython")
  ;; :general
  ;; (yc/leader-keys-major-mode
  
  ;;   "'"  'python-start-or-switch-repl
  ;;   "="  'yapfify-buffer
  ;;   "c"  '(:ignore t :which-key "execute")
  ;;   "cc" 'python-execute-file
  ;;   "cC" 'python-execute-file-focus
  ;;   "d"  '(:ignore t :which-key "debug")
  ;;   "db" 'python-toggle-breakpoint  ; check in spacemacs
  ;;   "h"  '(:ignore t :which-key "help")
  ;;   "hh" 'anaconda-mode-show-doc
  ;;   ;; "hd" 'helm-pydoc
  ;;   "g"  '(:ignore t :which-key "goto")
  ;;   "ga" 'anaconda-mode-find-assignments
  ;;   "gb" 'anaconda-mode-go-back
  ;;   "gu" 'anaconda-mode-find-references
  ;;   "s"  '(:ignore t :which-key "send to REPL")
  ;;   "sB" 'python-shell-send-buffer-switch
  ;;   "sb" 'python-shell-send-buffer
  ;;   "sF" 'python-shell-send-defun-switch
  ;;   "sf" 'python-shell-send-defun
  ;;   "si" 'python-start-or-switch-repl
  ;;   "sR" 'python-shell-send-region-switch
  ;;   "sr" 'python-shell-send-region
  ;;   "r"  '(:ignore t :which-key "refactor")
  ;;   "ri" 'python-remove-unused-imports  ; in spacemacs
  ;;   "rI" 'py-isort-buffer
  ;;   "v"  '(:ignore t :which-key "pyenv")
  ;;   "vu" 'pyenv-mode-unset
  ;;   "vs" 'pyenv-mode-set
  ;;   "V"  '(:ignore t :which-key "pyvenv")
  ;;   "Va" 'pyvenv-activate
  ;;   "Vd" 'pyvenv-deactivate
  ;;   "Vw" 'pyvenv-workon)
  )

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

(use-package anaconda-mode
  :after python
  :init
  (setq anaconda-mode-installation-directory
        (concat yc/cache-dir "/anaconda-mode"))
  :hook
  (python-mode . anaconda-mode)
  (python-mode . anaconda-eldoc-mode))

(use-package company-anaconda
  :after (anaconda python)
  :config
  (with-eval-after-load 'company
    (add-to-list 'company-backends '(company-anaconda :with company-capf))))

(use-package elpy
  :after python
  :config
  (elpy-enable)
  (major-mode-hydra-define+ python-mode nil
    ("Python"
     (("i" elpy-importmagic-fixup "Importmagic fixup")
      ("d" elpy-goto-definition   "Goto definition")
      ("r" elpy-multiedit-python-symbol-at-point   "Rename symbol")
      ("f" elpy-format-code   "Format code")))))

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
  :hook (python-mode . yapf-mode))

(use-package live-py-mode
  :after python)

(provide 'lang-python)
