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
  (defhydra hydra-vimish-fold (:color red :hint nil)
    "
 _f_: fold  _u_: unfold  _r_: refold  _t_: toggle  _d_: delete    _n_: next      _q_: quit
          _U_: Unfold  _R_: Refold  _T_: Toggle  _D_: Delete    _p_: previous
  "
    ("f" vimish-fold)
    ("u" vimish-fold-unfold)
    ("r" vimish-fold-refold)
    ("t" vimish-fold-toggle)
    ("d" vimish-fold-delete)
    ("U" vimish-fold-unfold-all)
    ("R" vimish-fold-refold-all)
    ("T" vimish-fold-toggle-all)
    ("D" vimish-fold-delete-all)
    ("n" vimish-fold-next-fold)
    ("p" vimish-fold-previous-fold)
    ("q" nil :color blue)))

(pretty-hydra-define hydra-fold
  (:hint nil :foreign-keys warn :quit-key "q")
  ("Fold"
   (("q" nil "Quit"))
   "Do" (("f" vimish-fold)
         ("k" vimish-fold-delete)
         ("K" vimish-fold-delete-all))
   "Jump"
   (("<tab>" vimish-fold-toggle)
    ("S-<tab>" vimish-fold-toggle-all))
   "Toggle"
   (("<" vimish-fold-previous-fold)
    (">" vimish-fold-next-fold))))

(use-package origami
  ;;:hook (prog-mode . origami-mode)
  :pretty-hydra
  ((:title (pretty-hydra-title "Origami" 'octicon "fold" :height 1.1 :v-adjust -0.05)
           :color amaranth :quit-key "q")
   ("Node"
    ((":" origami-recursively-toggle-node "toggle recursively")
     ("a" origami-toggle-all-nodes "toggle all")
     ("t" origami-toggle-node "toggle current")
     ("o" origami-show-only-node "only show current"))
    "Actions"
    (("u" origami-undo "undo")
     ("d" origami-redo "redo")
     ("r" origami-reset "reset"))))
  :bind (:map origami-mode-map
              ("C-`" . origami-hydra/body))
  :init (setq origami-show-fold-header t)
  :config
  (global-origami-mode t)
  (face-spec-reset-face 'origami-fold-header-face)
  (defhydra hydra-origami (:color red)
    "
  _o_pen node    _n_ext fold       toggle _f_orward
  _c_lose node   _p_revious fold   toggle _a_ll
  "
    ("o" origami-open-node)
    ("c" origami-close-node)
    ("n" origami-next-fold)
    ("p" origami-previous-fold)
    ("f" origami-forward-toggle-node)
    ("a" origami-toggle-all-nodes)
    ("t" origami-toggle-node)))

;; origami-predef

;; lsp =textDocument/foldingRange=
;; - https://github.com/emacs-lsp/lsp-origami
(use-package lsp-origami
  :after lsp
  :hook
  (lsp-after-open . lsp-origami-try-enable))

(provide 'init-folding)
