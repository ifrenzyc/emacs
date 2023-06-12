;; init-tab-bar.el --- Initialize tab-bar settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; - https://github.com/ema2159/centaur-tabs
;; - https://github.com/manateelazycat/awesome-tab
;; 

;;; Code

(use-package centaur-tabs
  :disabled t
  ;; :commands (centaur-tabs-mode)
  ;; :hook
  ;; (after-init . centaur-tabs-mode)
  :custom
  (centaur-tabs-projectile-buffer-group-calc t)
  ;; (centaur-tabs-enable-buffer-reordering)
  ;; (setq centaur-tabs-adjust-buffer-order t)
  ;; (centaur-tabs-change-fonts "arial" 160)
  (uniquify-separator "/")
  (uniquify-buffer-name-style 'forward)
  :config
  (setq centaur-tabs-style "bar"
        centaur-tabs-height 32
        centaur-tabs-set-icons t
        centaur-tabs-set-modified-marker t
        centaur-tabs-show-navigation-buttons t
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-set-bar 'under
        x-underline-at-descent-line t)
  (centaur-tabs-headline-match)
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

 Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
 All buffer name start with * will group to \"Emacs\".
 Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ;; ((not (eq (file-remote-p (buffer-file-name)) nil))
      ;; "Remote")
      ((or (string-equal "*" (substring (buffer-name) 0 1))
           (memq major-mode '(magit-process-mode
                              magit-status-mode
                              magit-diff-mode
                              magit-log-mode
                              magit-file-mode
                              magit-blob-mode
                              magit-blame-mode
                              )))
       "Emacs")
      ((derived-mode-p 'prog-mode)
       "Editing")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(helpful-mode
                          help-mode))
       "Help")
      ((memq major-mode '(org-mode
                          org-agenda-clockreport-mode
                          org-src-mode
                          org-agenda-mode
                          org-beamer-mode
                          org-indent-mode
                          org-bullets-mode
                          org-cdlatex-mode
                          org-agenda-log-mode
                          diary-mode))
       "OrgMode")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))
  ;; :hook
  ;; (dashboard-mode . centaur-tabs-local-mode)
  ;; (term-mode . centaur-tabs-local-mode)
  ;; (calendar-mode . centaur-tabs-local-mode)
  ;; (neotree-mode . centaur-tabs-local-mode)
  ;; (org-agenda-mode . centaur-tabs-local-mode)
  ;; (helpful-mode . centaur-tabs-local-mode)
  
  ("C-c t b" . centaur-tabs-backward)
  ("C-c t f" . centaur-tabs-forward)
  ("C-c t s" . centaur-tabs-counsel-switch-group)
  ("C-c t p" . centaur-tabs-group-by-projectile-project)
  ("C-c t g" . centaur-tabs-group-buffer-groups)
  (:map evil-normal-state-map
        ("g t" . centaur-tabs-forward)
        ("g T" . centaur-tabs-backward)))

(use-package sort-tab
  :disabled t
  :load-path "localelpa/sort-tab"
  :init
  (require 'sort-tab)
  :config
  (sort-tab-mode 1))

(provide 'init-tab-bar)
