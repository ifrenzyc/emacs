;; init-projectile.el --- Initialize projectile settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:
(require 'init-which-key)

(use-package projectile
  :bind
  (:map projectile-command-map
        ("."   . hydra-projectile/body)
        ("f"   . counsel-projectile-find-file)
        ("F"   . projectile-find-file-dwim-other-window)
        ("b"   . counsel-projectile-switch-to-buffer)
        ("B"   . projectile-switch-to-buffer-other-window)
        ("a"   . projectile-ag)
        ("C-r" . projectile-run-project)
        ("p"   . counsel-projectile-switch-project))
  (:map projectile-mode-map
        ("C-x p" . projectile-command-map))
  :commands (projectile-project-root)
  :custom
  ((projectile-enable-caching t)
   (projectile-completion-system 'ivy)
   (projectile-indexing-method 'native))
  :config
  (which-key-add-key-based-replacements "C-x p"   "Project & Projectile")
  ;; add to the globally ignored files
  (dolist (file-name '("*~" "*.elc" "*.class" "node_modules" "elpa" "localelpa"))
    (add-to-list 'projectile-globally-ignored-files file-name))
  (projectile-mode t)
  
  (defhydra hydra-projectile-other-window (:color red)
    "projectile-other-window"
    ("f"  projectile-find-file-other-window        "file")
    ("g"  projectile-find-file-dwim-other-window   "file dwim")
    ("d"  projectile-find-dir-other-window         "dir")
    ("b"  projectile-switch-to-buffer-other-window "buffer")
    ("q"  nil                                      "cancel" :color blue))

  (defhydra hydra-projectile (:color red :hint nil)
    "
       PROJECTILE: %(projectile-project-root)
                                                                                 ╭────────────┐
       Find File            Search/Tags          Buffers                Cache    │ Projectile │
    ╭────────────────────────────────────────────────────────────────────────────┴────────────╯
    _s-f_: file              _a_: ag                _i_: Ibuffer             _c_: cache clear
     _ff_: file dwim         _g_: update gtags      _b_: switch to buffer    _x_: remove known project
     _fd_: file curr dir     _o_: multi-occur     _s-k_: Kill all buffers    _X_: cleanup non-existing
      _r_: recent file                                                   ^^^^_z_: cache current
      _d_: dir
  "
    ("a"   projectile-ag)
    ("b"   projectile-switch-to-buffer)
    ("c"   projectile-invalidate-cache)
    ("d"   projectile-find-dir)
    ("s-f" projectile-find-file)
    ("ff"  projectile-find-file-dwim)
    ("fd"  projectile-find-file-in-directory)
    ("g"   ggtags-update-tags)
    ("s-g" ggtags-update-tags)
    ("i"   projectile-ibuffer)
    ("K"   projectile-kill-buffers)
    ("s-k" projectile-kill-buffers)
    ("m"   projectile-multi-occur)
    ("o"   projectile-multi-occur)
    ("s-p" projectile-switch-project "switch project")
    ("p"   projectile-switch-project)
    ("s"   projectile-switch-project)
    ("r"   projectile-recentf)
    ("x"   projectile-remove-known-project)
    ("X"   projectile-cleanup-known-projects)
    ("z"   projectile-cache-current-file)
    ("`"   hydra-projectile-other-window/body "other window")
    ("q"   nil "cancel" :color blue)))

(provide 'init-projectile)
;;; init-projectile.el ends here
