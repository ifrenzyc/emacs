;; init-buffer.el --- Initialize buffer configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:
;; Find a recent file using Ido.
;; mapping key to =C-c f= .
;; Save a list of recent files visited. (open recent file with C-c f)
(use-package recentf
  :hook
  (after-init . recentf-mode)
  :init
  ;; Save a list of recent files visited. (open recent file with C-x f)
  (setq recentf-max-saved-items 1000   ; just 1000 is too recent
        recentf-exclude
        '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
          "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
          "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
          "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
          (lambda (file) (file-in-directory-p file package-user-dir))))
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude))

(use-package nerd-icons-ibuffer
  :hook
  (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package ibuffer
  :bind
  (([remap list-buffers] . ibuffer))
  :custom
  (ibuffer-expert t)
  (ibuffer-show-empty-filter-groups nil)
  :config
  (setq ibuffer-formats
	    '((mark modified read-only " "
		        (name 40 60 :left :elide) ; change: 60s were originally 18s
		        " "
		        (size 9 -1 :right)
		        " "
		        (mode 16 16 :left :elide)
		        " " filename-and-process)
	      (mark " "
		        (name 16 -1)
                " " filename))))

(use-package ibuffer-vc
  :after ibuffer
  :bind
  (:map ibuffer-mode-map
        ("/ V" . ibuffer-vc-set-filter-groups-by-vc-root)))

(use-package ibuffer-projectile
  :after projectile
  :hook
  (ibuffer . (lambda ()
               (ibuffer-projectile-set-filter-groups)
               (unless (eq ibuffer-sorting-mode 'alphabetic)
                 (ibuffer-do-sort-by-project-name)))))

(use-package bufler
  :config
  (bufler-mode)
  (bufler-workspace-mode))

(use-package unmodified-buffer
  :hook
  (after-init . unmodified-buffer-mode))

(use-package all-the-icons-ibuffer
  :disabled t
  :hook
  (ibuffer-mode . all-the-icons-ibuffer-mode)
  :init
  ;; Use human readable file size in ibuffer.
  (setq all-the-icons-ibuffer-human-readable-size t))

(provide 'init-buffer)
;;; init-buffer.el ends here
