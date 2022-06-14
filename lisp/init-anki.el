;; init-anki.el --- Initialize anki settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; - https://github.com/chenyanming/anki.el
;; - https://github.com/louietan/anki-editor
;; 

;;; Code

(use-package anki
  :load-path "localelpa/anki/"
  :init
  (add-hook 'anki-mode-hook #'shrface-mode)
  (add-hook 'anki-card-mode-hook #'shrface-mode)
  (autoload 'anki "anki")
  (autoload 'anki-browser "anki")
  (autoload 'anki-list-decks "anki")
  :config
  ;; (require 'shrface) ; If you use shrface, require it here
  (setq anki-shr-rendering-functions (append anki-shr-rendering-functions shr-external-rendering-functions))
  (setq sql-sqlite-program "/usr/bin/sqlite3")
  ;; Set up the collection directory, which should contain a file - collection.anki2 and a folder - collection.media
  (setq anki-collection-dir "/Users/yangc/Library/Application Support/Anki2/User 1"))

(provide 'init-anki)
