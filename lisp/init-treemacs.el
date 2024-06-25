;; init-treemacs.el --- Initialize treemacs settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; - https://github.com/Alexander-Miller/treemacs
;; - https://blog.jft.rocks/emacs/treemacs-icons.html

;;; Code:
(use-package treemacs
  :bind
  (("M-0"       . treemacs)
   ([f8]        . treemacs-select-window)
   ("C-x t 1"   . treemacs-delete-other-windows)
   ("C-x t B"   . treemacs-bookmark)
   ("C-x t C-t" . treemacs-find-file)
   ("C-x t M-t" . treemacs-find-tag))
  (:map treemacs-mode-map
        ("SPC" . (lambda () (interactive) (treemacs-visit-node-no-split t)))  ;; similar to neotree-quick-look
        ("j"   . treemacs-next-line)
        ("A"   . mine/toggle-maximize-treemacs)
        ("k"   . treemacs-previous-line)
        ("H"   . hydra-move-splitter-left-4x)
        ("L"   . hydra-move-splitter-right-4x))

  ;; (:map treemacs-mode-map
  ;;       ([mouse-1]   . treemacs-single-click-expand-action))
  ;; :init
  ;; :commands (treemacs-follow-mode
  ;;            treemacs-filewatch-mode
  ;;            treemacs-fringe-indicator-mode
  ;;            treemacs-git-mode)
  :custom-face
  (treemacs-window-background-face ((t (:background "#efe9dd" :height 160))))
  :custom
  (treemacs-collapse-dirs                 (if (treemacs--find-python3) 3 0))
  (treemacs-deferred-git-apply-delay      0.5)
  (treemacs-display-in-side-window        t)
  (treemacs-eldoc-display                 t)
  (treemacs-file-event-delay              5000)
  (treemacs-file-follow-delay             0.2)
  (treemacs-follow-after-init             t)
  (treemacs-git-command-pipe              "")
  (treemacs-goto-tag-strategy             'refetch-index)
  (treemacs-indentation                   2)
  (treemacs-indentation-string            " ")
  (treemacs-is-never-other-window         nil)
  (treemacs-max-git-entries               5000)
  (treemacs-missing-project-action        'ask)
  (treemacs-no-png-images                 nil)
  (treemacs-no-delete-other-windows       t)
  (treemacs-project-follow-cleanup        nil)
  (treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory))
  (treemacs-recenter-distance             0.1)
  (treemacs-recenter-after-file-follow    nil)
  (treemacs-recenter-after-tag-follow     nil)
  (treemacs-recenter-after-project-jump   'always)
  (treemacs-recenter-after-project-expand 'on-distance)
  (treemacs-show-cursor                   nil)
  (treemacs-show-hidden-files             t)
  (treemacs-silent-filewatch              nil)
  (treemacs-silent-refresh                nil)
  (treemacs-sorting                       'alphabetic-asc)
  (treemacs-space-between-root-nodes      t)
  (treemacs-tag-follow-cleanup            t)
  (treemacs-tag-follow-delay              1.5)
  (treemacs-width                         35)
  :hook
  (treemacs-mode . (lambda ()
                     (setq-local line-spacing 0.4)
                     (setq-local truncate-lines t)
                     (setq-local word-wrap nil)))
  :config
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "<f8>") #'treemacs-select-window))

  ;; https://github.com/Alexander-Miller/treemacs/issues/439
  (defun mine/toggle-maximize-treemacs ()
    "Maximize/restore Treemacs buffer."
    (interactive)
    (unless (boundp 'treemacs--original-width)
      (setq treemacs--original-width treemacs-width))
    (with-selected-window (treemacs-get-local-window)
      (setq treemacs-width
            (if (= treemacs-width treemacs--original-width)
                (/ (frame-width) 2)
              treemacs--original-width))
      (treemacs--set-width treemacs-width)))

  ;; The default width and height of the icons is 22 pixels. If you are
  ;; using a Hi-DPI display, uncomment this to double the icon size.
  ;;(treemacs-resize-icons 44)

  (treemacs-follow-mode nil)
  (treemacs-filewatch-mode t)
  (treemacs-hide-gitignored-files-mode nil)
  (treemacs-fringe-indicator-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (treemacs--find-python3))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple))))

(use-package treemacs-nerd-icons
  :demand t
  :load-path "localelpa/treemacs-nerd-icons"
  :custom-face
  (treemacs-nerd-icons-root-face ((t (:inherit nerd-icons-green :height 1.3))))
  (treemacs-nerd-icons-file-face ((t (:inherit nerd-icons-dsilver))))
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package treemacs-projectile
  :after treemacs projectile
  :bind (([M-f8] . treemacs-projectile)
         :map projectile-command-map
         ("h" . treemacs-projectile)))

(use-package treemacs-magit
  :after treemacs magit)

(use-package lsp-treemacs
  :after treemacs lsp
  :bind (:map lsp-mode-map
              ("M-9" . lsp-treemacs-errors-list))
  :init
  (lsp-treemacs-sync-mode t))

(provide 'init-treemacs)
;;; init-treemacs.el ends here
