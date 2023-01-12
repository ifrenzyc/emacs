;; init-tree-sitter.el --- Initialize tree-sitter settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:

(require 'treesit)
(when (treesit-available-p)
  (setq major-mode-remap-alist
        '((bash-mode       . bash-ts-mode)
          (c++-mode        . c++-ts-mode)
          (c-mode          . c-ts-mode)
          (cmake-mode      . cmake-ts-mode)
          (csharp-mode     . csharp-ts-mode)
          (css-mode        . css-ts-mode)
          (dockerfile-mode . dockerfile-ts-mode)
          (go-mod-mode     . go-mod-ts-mode)
          (go-mode         . go-ts-mode)
          (java-mode       . java-ts-mode)
          (js-mode         . js-ts-mode)
          (json-mode       . json-ts-mode)
          (ng2-mode        . ng2-ts-mode)
          (python-mode     . python-ts-mode)
          (ruby-mode       . ruby-ts-mode)
          (rust-mode       . rust-ts-mode)
          (toml-mode       . toml-ts-mode)
          (tsx-mode        . tsx-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (yaml-mode       . yaml-ts-mode))))

(use-package treesit
  :ensure nil
  :init
  ;; https://www.nathanfurnal.xyz/posts/building-tree-sitter-langs-emacs/
  ;; https://www.reddit.com/r/emacs/comments/zzf9n6/building_treesitter_languages_for_emacs/
  (setq treesit-language-source-alist
        '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (c . ("https://github.com/tree-sitter/tree-sitter-c"))
          (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (css . ("https://github.com/tree-sitter/tree-sitter-css"))
          (go . ("https://github.com/tree-sitter/tree-sitter-go"))
          (html . ("https://github.com/tree-sitter/tree-sitter-html"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          (lua . ("https://github.com/Azganoth/tree-sitter-lua"))
          (make . ("https://github.com/alemuller/tree-sitter-make"))
          (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" "ocaml/src" "ocaml"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))
          (php . ("https://github.com/tree-sitter/tree-sitter-php"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "typescript/src" "typescript"))
          (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
          (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
          (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (zig . ("https://github.com/GrayJack/tree-sitter-zig"))))
  :config
  (defun nf/treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
	    (treesit-install-language-grammar lang)
	    (message "`%s' parser was installed." lang)
	    (sit-for 0.75)))))

(use-package tree-sitter
  :init
  (global-tree-sitter-mode)
  :hook (tree-sitter-after-on . tree-sitter-hl-mode))

(use-package tree-sitter-langs :after tree-sitter)

(use-package fringe-helper)

(use-package ts-fold
  :load-path "localelpa/ts-fold"
  :hook
  (tree-sitter-after-on . ts-fold-mode)
  (tree-sitter-after-on . ts-fold-indicators-mode))

(provide 'init-tree-sitter)
