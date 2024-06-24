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
  :pretty-hydra
  (hydra-fold
   (:title (pretty-hydra-title "Folding" 'octicon "fold" :height 1.1 :v-adjust -0.05)
           :pre (global-origami-mode t) :color pink :quit-key "C-g")
   ("Move"
    (("C-a" mwim-beginning-of-code-or-line "⭰")
     ("C-e" mwim-end-of-code-or-line "⭲")
     ("C-b" backward-char "←")
     ("C-n" next-line "↓")
     ("C-p" previous-line "↑")
     ("C-f" forward-char "→")
     ("C-v" pager-page-down "↘")
     ("M-v" pager-page-up "↖")
     ("M-<" beginning-of-buffer "⭶")
     ("M->" end-of-buffer "⭸"))
    "Fold(vimish)"
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
    "Hideshow"
    (("}" hs-cycle "cycle block")
     ("{" hs-global-cycle)
     ("A" hs-toggle-all "toggle all")
     ("a" hs-show-all "show all")
     ("i" hs-hide-all "hide all")
     ("s" hs-show-block "show block")
     ("b" hs-hide-block "hide block")
     ("l" hs-hide-level "hide level"))
    "Origami Node"
    ((":" origami-recursively-toggle-node "toggle recursively")
     ("T" origami-toggle-all-nodes "toggle all")
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
  :init
  (setq-default vimish-fold-dir (expand-file-name ".vimish-fold/" yc/cache-dir))
  (vimish-fold-global-mode)
  :config
  (setq-default vimish-fold-header-width 119))

(use-package hideshow
  :bind
  (("C-<tab>"         . hs-cycle)
   ("C-<iso-lefttab>" . hs-global-cycle)
   ("C-S-<tab>"       . hs-global-cycle))
  :hook
  (prog-mode . hs-minor-mode)
  :config
  (setq hs-hide-comments-when-hiding-all nil
        ;; Nicer code-folding overlays (with fringe indicators)
        hs-set-up-overlay #'hideshow-set-up-overlay-fn)

  (defface hideshow-folded-face
    `((t (:inherit font-lock-comment-face :weight light)))
    "Face to hightlight `hideshow' overlays."
    :group 'hideshow)
  
  (defun hideshow-set-up-overlay-fn (ov)
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put
       ov 'display (propertize "  [...]  " 'face 'hideshow-folded-face))))
  
  (dolist (hs-command (list #'hs-cycle
                            #'hs-global-cycle))
    (advice-add hs-command :before
                (lambda (&optional end) "Advice to ensure `hs-minor-mode' is enabled"
                  (unless (bound-and-true-p hs-minor-mode)
                    (hs-minor-mode +1)))))

  (defun hs-cycle (&optional level)
    (interactive "p")
    (save-excursion
      (if (= level 1)
          (pcase last-command
            ('hs-cycle
             (hs-hide-level 1)
             (setq this-command 'hs-cycle-children))
            ('hs-cycle-children
             ;;TODO: Fix this case. `hs-show-block' needs to be called twice to
             ;;open all folds of the parent block.
             (hs-show-block)
             (hs-show-block)
             (setq this-command 'hs-cycle-subtree))
            ('hs-cycle-subtree
             (hs-hide-block))
            (_
             (if (not (hs-already-hidden-p))
                 (hs-hide-block)
               (hs-hide-level 1)
               (setq this-command 'hs-cycle-children))))
        (hs-hide-level level)
        (setq this-command 'hs-hide-level))))
  
  (defun hs-global-cycle ()
    (interactive)
    (pcase last-command
      ('hs-global-cycle
       (save-excursion (hs-show-all))
       (setq this-command 'hs-global-show))
      (_ (hs-hide-all)))))

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
