;; init-treemacs.el --- Initialize treemacs settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; - https://github.com/Alexander-Miller/treemacs
;; - https://blog.jft.rocks/emacs/treemacs-icons.html

;;; Code:
(use-package treemacs
  ;; :init
  ;; :commands (treemacs-follow-mode
  ;;            treemacs-filewatch-mode
  ;;            treemacs-fringe-indicator-mode
  ;;            treemacs-git-mode)
  :custom-face
  (treemacs-window-background-face ((t (:background "#efe9dd" :height 140))))
  :hook
  ;; init treemacs in emacs keys;
  ;; (treemacs-mode . evil-normalize-keymaps)
  (treemacs-mode . (lambda ()
                     (setq-local line-spacing 0.4)
                     (setq-local truncate-lines t)
                     (setq-local word-wrap nil)))
  :config
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  
  (setq treemacs-collapse-dirs                 (if (treemacs--find-python3) 3 0)
        treemacs-deferred-git-apply-delay      0.5
        treemacs-display-in-side-window        t
        treemacs-eldoc-display                 t
        treemacs-file-event-delay              5000
        treemacs-file-follow-delay             0.2
        treemacs-follow-after-init             t
        treemacs-git-command-pipe              ""
        treemacs-goto-tag-strategy             'refetch-index
        treemacs-indentation                   2
        treemacs-indentation-string            " "
        treemacs-is-never-other-window         nil
        treemacs-max-git-entries               5000
        treemacs-missing-project-action        'ask
        treemacs-no-png-images                 nil
        treemacs-no-delete-other-windows       t
        treemacs-project-follow-cleanup        nil
        treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
        treemacs-recenter-distance             0.1
        treemacs-recenter-after-file-follow    nil
        treemacs-recenter-after-tag-follow     nil
        treemacs-recenter-after-project-jump   'always
        treemacs-recenter-after-project-expand 'on-distance
        treemacs-show-cursor                   nil
        treemacs-show-hidden-files             t
        treemacs-silent-filewatch              nil
        treemacs-silent-refresh                nil
        treemacs-sorting                       'alphabetic-asc
        treemacs-space-between-root-nodes      t
        treemacs-tag-follow-cleanup            t
        treemacs-tag-follow-delay              1.5
        treemacs-width                         35)

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
     (treemacs-git-mode 'simple)))

  ;; ;; Improve treemacs icons
  ;; (with-eval-after-load 'treemacs
  ;;   (with-eval-after-load 'all-the-icons
  ;;     (let ((all-the-icons-default-adjust 0)
  ;;           (tab-width 1))
  ;;       ;; Root icon
  ;;       (setq treemacs-icon-root-png
  ;;             (concat (all-the-icons-octicon "repo" :height 0.8 :v-adjust -0.2)  " "))
  ;;       ;; File icons
  ;;       (setq treemacs-icon-open-png
  ;;             (concat
  ;;              (all-the-icons-octicon "chevron-down" :height 0.8 :v-adjust 0.1)
  ;;              "\t"
  ;;              (all-the-icons-octicon "file-directory" :v-adjust 0)
  ;;              "\t")
  ;;             treemacs-icon-closed-png
  ;;             (concat
  ;;              (all-the-icons-octicon "chevron-right" :height 0.8
  ;;                                     :v-adjust 0.1 :face 'font-lock-doc-face)
  ;;              "\t"
  ;;              (all-the-icons-octicon "file-directory" :v-adjust 0 :face 'font-lock-doc-face)
  ;;              "\t"))
  ;;       ;; File type icons
  ;;       (setq treemacs-icons-hash (make-hash-table :size 200 :test #'equal)
  ;;             treemacs-icon-fallback (concat
  ;;                                     "\t\t"
  ;;                                     (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver
  ;;                                                           :height 0.8 :v-adjust 0.0)
  ;;                                     "\t")
  ;;             treemacs-icon-text treemacs-icon-fallback)

  ;;       (dolist (item all-the-icons-icon-alist)
  ;;         (let* ((extension (car item))
  ;;                (func (cadr item))
  ;;                (args (append (list (caddr item)) '(:v-adjust -0.05) (cdddr item)))
  ;;                (icon (apply func args))
  ;;                (key (s-replace-all '(("^" . "") ("\\" . "") ("$" . "") ("." . "")) extension))
  ;;                (value (concat "\t\t" icon "\t")))
  ;;           (unless (ht-get treemacs-icons-hash (s-replace-regexp "\\?" "" key))
  
  ;;           (unless (ht-get treemacs-icons-hash (s-replace-regexp ".\\?" "" key))
  ;;             (ht-set! treemacs-icons-hash (s-replace-regexp ".\\?" "" key) value)))))))
  :bind
  (([f8]        . treemacs)
   ("M-0"       . treemacs-select-window)
   ("C-x t 1"   . treemacs-delete-other-windows)
   ("C-x t B"   . treemacs-bookmark)
   ("C-x t C-t" . treemacs-find-file)
   ("C-x t M-t" . treemacs-find-tag))
  (:map treemacs-mode-map
        ("j" . treemacs-next-line)
        ("A" . mine/toggle-maximize-treemacs)
        ("k" . treemacs-previous-line)
        ("SPC" . (lambda () (interactive) (treemacs-visit-node-no-split t)))  ;; similar to neotree-quick-look
        ("H" . hydra-move-splitter-left-2x)
        ("L" . hydra-move-splitter-right-2x))

  ;; (:map treemacs-mode-map
  ;;       ([mouse-1]   . treemacs-single-click-expand-action))
  )

(use-package treemacs-nerd-icons
  :demand t
  :load-path "localelpa/treemacs-nerd-icons"
  :custom-face
  (treemacs-nerd-icons-root-face ((t (:inherit nerd-icons-green :height 1.3))))
  (treemacs-nerd-icons-file-face ((t (:inherit nerd-icons-dsilver))))
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package treemacs-evil
  :disabled t
  :after (evil treemacs)
  :bind
  (:map evil-treemacs-state-map
        ("H" . hydra-move-splitter-left-2x)
        ("L" . hydra-move-splitter-right-2x)
        ))

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
  (lsp-treemacs-sync-mode t)
  ;; :config
  ;; (with-no-warnings
  ;;   (when (require 'all-the-icons nil t)
  ;;     (treemacs-create-theme "centaur-colors"
  ;;       :extends "doom-colors"
  ;;       :config
  ;;       (progn
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-octicon "repo" :height 1.0 :v-adjust -0.1 :face 'all-the-icons-blue))
  ;;          :extensions (root))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue))
  ;;          :extensions (boolean-data))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-material "settings_input_component" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-orange))
  ;;          :extensions (class))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-material "palette" :height 0.95 :v-adjust -0.15))
  ;;          :extensions (color-palette))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-faicon "square-o" :height 0.95 :v-adjust -0.15))
  ;;          :extensions (constant))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-faicon "file-text-o" :height 0.95 :v-adjust -0.05))
  ;;          :extensions (document))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-material "storage" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-orange))
  ;;          :extensions (enumerator))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-material "format_align_right" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue))
  ;;          :extensions (enumitem))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-faicon "bolt" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-orange))
  ;;          :extensions (event))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue))
  ;;          :extensions (field))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-faicon "search" :height 0.95 :v-adjust -0.05))
  ;;          :extensions (indexer))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-material "filter_center_focus" :height 0.95 :v-adjust -0.15))
  ;;          :extensions (intellisense-keyword))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-material "share" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue))
  ;;          :extensions (interface))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue))
  ;;          :extensions (localvariable))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-faicon "cube" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-purple))
  ;;          :extensions (method))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-material "view_module" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue))
  ;;          :extensions (namespace))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-material "format_list_numbered" :height 0.95 :v-adjust -0.15))
  ;;          :extensions (numeric))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-material "control_point" :height 0.95 :v-adjust -0.2))
  ;;          :extensions (operator))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.05))
  ;;          :extensions (property))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-material "format_align_center" :height 0.95 :v-adjust -0.15))
  ;;          :extensions (snippet))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-faicon "text-width" :height 0.9 :v-adjust -0.05))
  ;;          :extensions (string))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-orange))
  ;;          :extensions (structure))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-material "format_align_center" :height 0.95 :v-adjust -0.15))
  ;;          :extensions (template))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-octicon "chevron-right" :height 0.75 :v-adjust 0.1 :face 'font-lock-doc-face))
  ;;          :extensions (collapsed) :fallback "+")
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-octicon "chevron-down" :height 0.75 :v-adjust 0.1 :face 'font-lock-doc-face))
  ;;          :extensions (expanded) :fallback "-")
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-octicon "file-binary" :height 0.9  :v-adjust 0.0 :face 'font-lock-doc-face))
  ;;          :extensions (classfile))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-blue))
  ;;          :extensions (default-folder-opened))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-blue))
  ;;          :extensions (default-folder))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-green))
  ;;          :extensions (default-root-folder-opened))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-green))
  ;;          :extensions (default-root-folder))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-octicon "file-binary" :height 0.9 :v-adjust 0.0 :face 'font-lock-doc-face))
  ;;          :extensions ("class"))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-octicon "file-zip" :height 0.9 :v-adjust 0.0 :face 'font-lock-doc-face))
  ;;          :extensions (file-type-jar))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
  ;;          :extensions (folder-open))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'font-lock-doc-face))
  ;;          :extensions (folder))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-orange))
  ;;          :extensions (folder-type-component-opened))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-orange))
  ;;          :extensions (folder-type-component))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-green))
  ;;          :extensions (folder-type-library-opened))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-green))
  ;;          :extensions (folder-type-library))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-pink))
  ;;          :extensions (folder-type-maven-opened))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-pink))
  ;;          :extensions (folder-type-maven))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05 :face 'font-lock-type-face))
  ;;          :extensions (folder-type-package-opened))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'font-lock-type-face))
  ;;          :extensions (folder-type-package))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-faicon "plus" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
  ;;          :extensions (icon-create))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-faicon "list" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
  ;;          :extensions (icon-flat))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-material "share" :height 0.95 :v-adjust -0.2 :face 'all-the-icons-lblue))
  ;;          :extensions (icon-hierarchical))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-faicon "link" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
  ;;          :extensions (icon-link))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-faicon "refresh" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
  ;;          :extensions (icon-refresh))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-faicon "chain-broken" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
  ;;          :extensions (icon-unlink))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-alltheicon "java" :height 1.0 :v-adjust 0.0 :face 'all-the-icons-orange))
  ;;          :extensions (jar))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-faicon "book" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-green))
  ;;          :extensions (library))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-faicon "folder-open" :face 'all-the-icons-lblue))
  ;;          :extensions (packagefolder-open))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue))
  ;;          :extensions (packagefolder))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-faicon "archive" :height 0.9 :v-adjust -0.05 :face 'font-lock-doc-face))
  ;;          :extensions (package))
  ;;         (treemacs-create-icon
  ;;          :icon (format "%s " (all-the-icons-octicon "repo" :height 1.0 :v-adjust -0.1 :face 'all-the-icons-blue))
  ;;          :extensions (java-project))))

  ;;     (setq lsp-treemacs-theme "centaur-colors")))
  )

(provide 'init-treemacs)
;;; init-treemacs.el ends here
