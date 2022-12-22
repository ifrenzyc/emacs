;; init-neotree.el --- Initialize neotree settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; - https://github.com/emacs-evil/evil-collection/blob/master/evil-collection-neotree.el

;;; Code

(use-package neotree
  :commands (neotree-toggle)
  :hook
  ;; Use with evil mode
  ;; @see - https://www.emacswiki.org/emacs/NeoTree
  (neotree-mode . (lambda ()
                    (visual-line-mode -1)
                    (setq truncate-lines t)
                    (hl-line-mode 1)
                    (display-line-numbers-mode -1)    ; disable line-numbers minor mode for neotree
                    ))
  :bind
  (:map neotree-mode-map
        ("H" . hydra-move-splitter-left-2x)
        ("L" . hydra-move-splitter-right-2x))
  :config
  ;; 'classic, 'nerd, 'ascii, 'arrow
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-smart-open t)
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (setq neo-window-fixed-size nil)      ; 通过设置该参数，可以手动调整 neotree 窗口大小
  ;; Don't allow neotree to be the only open window
  (setq-default neo-dont-be-alone t)

  (defun neotree-copy-file ()
    (interactive)
    (let* ((current-path (neo-buffer--get-filename-current-line))
           (msg (format "Copy [%s] to: "
                        (neo-path--file-short-name current-path)))
           (to-path (read-file-name msg (file-name-directory current-path))))
      (dired-copy-file current-path to-path t))
    (neo-buffer--refresh t))


  ;; @see - https://github.com/jaypei/emacs-neotree/issues/218
  ;; @see - https://github.com/jaypei/emacs-neotree/issues/55
  ;; (custom-set-faces
  ;;  '(neo-dir-link-face ((t (:foreground "deep sky blue" :slant normal :weight bold :height 120 :family "Fantasque Sans Mono"))))
  ;;  '(neo-file-link-face ((t (:foreground "White" :weight normal :height 120 :family "Fantasque Sans Mono")))))

  (defun text-scale-twice ()
    (interactive)
    (progn
      (text-scale-adjust 0)
      (text-scale-decrease 2)))
  ;; (add-hook 'neo-after-create-hook (lambda (_)(call-interactively 'text-scale-twice)))

  (defvar jp-neotree--title (with-faicon "tree" "neotree" 1 -0.05))
  (pretty-hydra-define hydra-neotree-new
    (:hint nil :color red :foreign-keys warn :quit-key "q" :title jp-neotree--title :separator "═")
    ("Navigation"
     (("k" neotree-previous-line "↑")
      ("j" neotree-next-line "↓"))
     "Resize"
     (("H" hydra-move-splitter-left-2x "←")
      ("L" hydra-move-splitter-right-2x "→"))
     "Enter"
     (("RET" neotree-enter "Open")
      ("o" neotree-enter "Open")
      ("SPC"  neotree-quick-look "Peek")
      ;; ("'" neotree-quick-look "Quick Look")
      ("-" neotree-enter-horizontal-split "Horizontal Split")
      ("|" neotree-enter-vertical-split "Vertical Split"))
     "Action"
     (("gr" neotree-refresh "Refresh")
      ("R" neotree-change-root "Change Root")
      ("C" neotree-create-node "Create")
      ("c" neotree-copy-node "Copy")
      ("d" neotree-delete-node "Delete")
      ("r" neotree-rename-node "Rename or Move"))
     "Help"
     (("?" nil "close hints" :exit t)
      ("q" nil :exit t)
      ("s" neotree-hidden-file-toggle))
     ))

  (defhydra hydra-neotree (:color pink :hint nil)
    "
                                                                                 ╭─────────┐
      Navigation   Resize     Enter                   Action              Help   │ NeoTree │
     ╭───────────────────────────────────────────────────────────────────────────┴─────────╯
           ^_k_^         ^ ^        [_RET_] open              [_gr_] refresh        [_s_]   hidden:^^^ %s(if neo-buffer--show-hidden-file-p \"on\" \"off\")
           ^^↑^^         ^ ^        [_o_] open                [_C_] create          [_?_]   close hints
           ^ ^      _H_ ←   → _L_   [_'_] quick look          [_c_] copy
           ^^↓^^         ^ ^        [_-_] horizonatal split   [_d_] delete
           ^_j_^         ^ ^        [_|_] vertical split      [_r_] rename or move
            ^ ^          ^ ^        ^ ^                     [_R_] change root
     --------------------------------------------------------------------------------
           "
    ("j" neotree-next-line)
    ("k" neotree-previous-line)
    ("H" hydra-move-splitter-left-2x)
    ("L" hydra-move-splitter-right-2x)
    ;; ("L" neotree-select-next-sibling-node)
    ;; ("H" neotree-select-previous-sibling-node)
    ;; ("j" neotree-next-line)
    ;; ("k" neotree-previous-line)
    ;; ("K" neotree-select-up-node)
    ;; ("J" neotree-select-down-node)

    ("RET" neotree-enter)
    ("o" neotree-enter)
    ("'" neotree-quick-look)
    ("-" neotree-enter-horizontal-split)
    ("|" neotree-enter-vertical-split)

    ("gr" neotree-refresh)
    ("C" neotree-create-node)
    ("c" neotree-copy-node)
    ("d" neotree-delete-node)
    ("r" neotree-rename-node)
    ("R" neotree-change-root)
    ;; ("h" spacemacs/neotree-collapse-or-up)
    ;; ("l" spacemacs/neotree-expand-or-open)
    ("?" nil :exit t)
    ("q" nil :exit t)
    ("s" neotree-hidden-file-toggle))

  ;; ("TAB" neotree-stretch-toggle)
  ;;   "
  ;; Navigation^^^^             Actions^^         Visual actions/config^^^
  ;; ───────^^^^─────────────── ───────^^──────── ───────^^^────────────────
  ;; [_L_]   next sibling^^     [_c_] create      [_TAB_] shrink/enlarge
  ;; [_H_]   previous sibling^^ [_C_] copy        [_|_]   vertical split
  ;; [_J_]   goto child^^       [_d_] delete      [_-_]   horizonatal split
  ;; [_K_]   goto parent^^      [_r_] rename      [_gr_]  refresh^
  ;; [_l_]   open/expand^^      [_R_] change root [_s_]   hidden:^^^ %s(if neo-buffer--show-hidden-file-p \"on\" \"off\")
  ;; [_h_]   up/collapse^^      ^^                ^^^
  ;; [_j_]   line down^^        ^^                ^^^
  ;; [_k_]   line up^^          ^^                ^^
  ;; [_'_]   quick look         ^^                ^^
  ;; [_RET_] open               ^^^^              [_?_]   close hints
  ;; "
  )
(provide 'init-neotree)
