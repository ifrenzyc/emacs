;; lang-clojure.el --- Initialize clojure settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code

(use-package clojure-mode
  :commands clojure-mode
  :init (add-to-list 'auto-mode-alist '("\\.\\(clj[sx]?\\|dtm\\|edn\\)\\'" . clojure-mode))
  :hook
  (clojure-mode . (lambda () (setq buffer-save-without-query t)))
  (clojure-mode . 'subword-mode)
  :config
  ;; Fancy docstrings for schema/defn when in the form:
  ;; (schema/defn NAME :- TYPE "DOCSTRING" ...)
  (put 'schema/defn 'clojure-doc-string-elt 4))

(use-package cider
  :after (clojure)
  :hook
  ((clojure-mode . cider-turn-on-eldoc-mode)
   (cider-repl-mode . subword-mode))
  :general
  (cider-repl-mode-map
   "M-RET" 'cider-doc)
  (cider-mode-map
   "M-RET" 'cider-doc)
  :config
  (setq cider-annotate-completion-candidates t
        cider-mode-line " cider"))

(use-package clj-refactor
  :after (clojure)
  :hook
  (clojure-mode . (lambda ()
                    (clj-refactor-mode 1)
                    (cljr-add-keybindings-with-prefix "C-c C-m")))
  :general
  (clojure-mode-map "C-:" 'clojure-toggle-keyword-string
                    "C->" 'cljr-cycle-coll))

(major-mode-hydra-bind clojure-mode "Connect"
  ("j" cider-jack-in "jack-in")
  ("J" cider-jack-in-clojurescript "jack-in-cljs")
  ("c" cider-connect "connect")
  ("R" cider-restart "restart")
  ("Q" cider-quit "quit"))

(major-mode-hydra-bind clojure-mode "Load"
  ("k" cider-load-buffer "buffer")
  ("l" cider-load-file "file")
  ("L" cider-load-all-project-ns "all-ns")
  ("r" cider-refresh "reload"))

(provide 'lang-clojure)
