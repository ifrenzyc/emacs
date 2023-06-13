;; init-folding.el --- Initialize folding configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; - https://github.com/mrkkrp/vimish-fold
;; This is a package to perform text folding like in Vim. It has the following features:
;; 
;; - folding of active regions;
;; - good visual feedback: it’s obvious which part of text is folded;
;; - persistence by default: when you close file your folds don’t disappear;
;; - persistence scales well, you can work on hundreds of files with lots of folds without adverse effects;
;; - it doesn’t break indentation or something;
;; - folds can be toggled from folded state to unfolded and back very easily;
;; - quick navigation between existing folds;
;; - you can use mouse to unfold folds (good for beginners and not only for them);
;; - for fans of avy package: you can use avy to fold text with minimal number of key strokes!
;; 
;; - origami
;; - hideshow
;; - vimish-fold
;; 

;;; Code:
(use-package vimish-fold
  :bind
  (:map vimish-fold-folded-keymap ("<tab>" . vimish-fold-unfold)
        :map vimish-fold-unfolded-keymap ("<tab>" . vimish-fold-refold))
  :init
  (setq-default vimish-fold-dir (expand-file-name ".vimish-fold/" yc/cache-dir))
  (vimish-fold-global-mode)
  :config
  (setq-default vimish-fold-header-width 119)
  :pretty-hydra
  (hydra-fold
   (:title (pretty-hydra-title "Folding" 'octicon "fold" :height 1.1 :v-adjust -0.05)
           :pre (global-origami-mode t) :color teal :quit-key "C-g")
   ("Fold(vimish)"
    (("f" vimish-fold)
     ("F" vimish-fold)
     ("u" vimish-fold-unfold)
     ("U" vimish-fold-unfold)
     ("r" vimish-fold-refold)
     ("R" vimish-fold-refold-all)
     ("d" vimish-fold-delete)
     ("D" vimish-fold-delete-all))
    "Jump(Vimish)"
    (("<" vimish-fold-previous-fold)
     (">" vimish-fold-next-fold))
    "Toggle(Vimish)"
    (("<tab>" vimish-fold-toggle)
     ("S-<tab>" vimish-fold-toggle-all))
    "Origami Node"
    ((":" origami-recursively-toggle-node "toggle recursively")
     ("a" origami-toggle-all-nodes "toggle all")
     ("t" origami-toggle-node "toggle current")
     ("o" origami-open-node)
     ("c" origami-close-node)
     ("n" origami-next-fold)
     ("p" origami-previous-fold)
     ("]" origami-forward-toggle-node)
     ("O" origami-show-only-node "only show current"))
    "Origami Actions"
    (("h" origami-undo "undo")
     ("k" origami-redo "redo")
     ("[" origami-reset "reset"))))
  )

(use-package origami
  :custom
  (origami-show-fold-header t)
  :config
  (global-origami-mode t)
  (face-spec-reset-face 'origami-fold-header-face))

;; origami-predef

;; lsp =textDocument/foldingRange=
;; - https://github.com/emacs-lsp/lsp-origami
(use-package lsp-origami
  :after lsp
  :hook
  (lsp-after-open . lsp-origami-try-enable))

(provide 'init-folding)
;;; init-folding.el ends here
