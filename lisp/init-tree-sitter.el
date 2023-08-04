;; init-tree-sitter.el --- Initialize tree-sitter settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:
(require 'treesit)

(use-package treesit
  :ensure nil
  :commands (treesit-install-language-grammar nf/treesit-install-all-languages)
  :init
  ;; https://www.nathanfurnal.xyz/posts/building-tree-sitter-langs-emacs/
  ;; https://www.reddit.com/r/emacs/comments/zzf9n6/building_treesitter_languages_for_emacs/
  (setq treesit-language-source-alist
        '((bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (c          . ("https://github.com/tree-sitter/tree-sitter-c"))
          (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (css        . ("https://github.com/tree-sitter/tree-sitter-css"))
          (cmake      . ("https://github.com/uyha/tree-sitter-cmake"))
          (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
          (elisp      . ("https://github.com/Wilfred/tree-sitter-elisp"))
          (go         . ("https://github.com/tree-sitter/tree-sitter-go"))
          (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod.git"))
          (html       . ("https://github.com/tree-sitter/tree-sitter-html"))
          (java       . ("https://github.com/tree-sitter/tree-sitter-java.git"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
          (lua        . ("https://github.com/Azganoth/tree-sitter-lua"))
          (make       . ("https://github.com/alemuller/tree-sitter-make"))
          (markdown   . ("https://github.com/MDeiml/tree-sitter-markdown" nil "tree-sitter-markdown/src"))
          (ocaml      . ("https://github.com/tree-sitter/tree-sitter-ocaml" nil "ocaml/src"))
          (org        . ("https://github.com/milisims/tree-sitter-org"))
          (python     . ("https://github.com/tree-sitter/tree-sitter-python"))
          (php        . ("https://github.com/tree-sitter/tree-sitter-php"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
          (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
          (ruby       . ("https://github.com/tree-sitter/tree-sitter-ruby"))
          (rust       . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (sql        . ("https://github.com/m-novikov/tree-sitter-sql"))
          (vue        . ("https://github.com/merico-dev/tree-sitter-vue"))
          (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml"))
          (toml       . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (zig        . ("https://github.com/GrayJack/tree-sitter-zig"))))
  :config
  (defun nf/treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
	    (treesit-install-language-grammar lang)
	    (message "`%s' parser was installed." lang)
	    (sit-for 0.75))))

  (when (treesit-available-p)
    (setq major-mode-remap-alist
          '((sh-mode         . bash-ts-mode)
            (c++-mode        . c++-ts-mode)
            (c-mode          . c-ts-mode)
            (cmake-mode      . cmake-ts-mode)
            (csharp-mode     . csharp-ts-mode)
            (css-mode        . css-ts-mode)
            (dockerfile-mode . dockerfile-ts-mode)
            (go-mod-mode     . go-mod-ts-mode)
            (go-mode         . go-ts-mode)
            (html-mode       . html-ts-mode)
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
            (yaml-mode       . yaml-ts-mode)))))

(use-package treesit-auto
  :config
  (add-to-list 'treesit-auto-fallback-alist '(bash-ts-mode . sh-mode)))

(use-package tree-sitter
  :init
  (global-tree-sitter-mode)
  :hook (tree-sitter-after-on . tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :disabled t
  :after tree-sitter
  :init
  ;; (tree-sitter-load 'vue)
  ;; (add-to-list 'tree-sitter-major-mode-language-alist '(web-mode . vue))
  (tree-sitter-load 'elisp)
  (add-to-list 'tree-sitter-major-mode-language-alist '(emacs-lisp-mode . elisp))
  (add-to-list 'tree-sitter-major-mode-language-alist '(inferior-emacs-lisp-mode . elisp))
  ;; (tree-sitter-load 'cmake)
  ;; (add-to-list 'tree-sitter-major-mode-language-alist '(cmake-mode . cmake))
  ;; (tree-sitter-load 'dockerfile)
  ;; (add-to-list 'tree-sitter-major-mode-language-alist '(dockerfile-mode . dockerfile))
  ;; (tree-sitter-load 'xml)
  ;; (add-to-list 'tree-sitter-major-mode-language-alist '(xml-mode . xml))
  (tree-sitter-load 'markdown)
  (add-to-list 'tree-sitter-major-mode-language-alist '(markdown-mode . markdown))
  (tree-sitter-load 'sql)
  (add-to-list 'tree-sitter-major-mode-language-alist '(sql-mode . sql)))

(use-package fringe-helper)

(use-package ts-fold
  :load-path "localelpa/ts-fold"
  :hook
  (tree-sitter-after-on . ts-fold-mode)
  (tree-sitter-after-on . ts-fold-indicators-mode)
  :init
  (require 'ts-fold-indicators))

(provide 'init-tree-sitter)
;;; init-tree-sitter.el ends here
