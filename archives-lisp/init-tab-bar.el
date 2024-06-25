;; init-tab-bar.el --- Initialize tab-bar settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; - https://github.com/ema2159/centaur-tabs
;; - https://github.com/manateelazycat/awesome-tab
;; 

;;; Code:

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

(use-package awesome-tab
  :load-path "localelpa/awesome-tab"
  :config
  (awesome-tab-mode t))

(use-package sort-tab
  ;; :disabled t
  :load-path "localelpa/sort-tab"
  :init
  (require 'sort-tab)
  :config
  (sort-tab-mode 1))

;; tabspaces - https://github.com/mclear-tools/tabspaces
(use-package project-tab-groups
  :init
  (setq project-tab-groups-tab-group-name-function #'+project-tab-groups-name-by-project-root)
  (project-tab-groups-mode t)
  :config
  (defun +project-tab-groups-name-by-project-root (dir)
    "Derive tab group name for project in DIR."
    (with-temp-buffer
      (setq default-directory dir)
      (hack-dir-local-variables-non-file-buffer)
      (let ((name (or (and (boundp 'tab-group-name) tab-group-name)
                      (and (boundp 'project-name) project-name)
                      (and (fboundp 'project-root)
                           (when-let ((project-current (project-current)))
                             (project-root project-current)))
                      (file-name-nondirectory (directory-file-name dir))))
            (name-template (or (and (boundp 'tab-group-name-template) tab-group-name-template)
                               (and (boundp 'project-name-template) project-name-template)
                               "%s")))
        (format name-template name)))))

(use-package tabspaces
  :hook (after-init . tabspaces-mode)
  :bind-keymap ("H-t" . tabspaces-command-map)
  :bind (:map tabspaces-command-map
              ("2" . tab-new)
              ("0" . tabspaces-close-workspace)
              ("p" . project-other-tab-command)
              ("k" . tabspaces-kill-buffers-close-workspace)
              ("DEL" . tabspaces-remove-current-buffer))
  :custom
  (tab-bar-show nil)

  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-initialize-project-with-todo nil)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*" "*Messages*"))
  ;; sessions
  ;; (tabspaces-session t)
  ;; (tabspaces-session-auto-restore t)
  :config
  ;; Ensure reading project list
  (require 'project)
  (project--ensure-read-project-list)
  
  (tabspaces-mode 1)
  
  ;; Rename the first tab to `tabspaces-default-tab'
  (tab-bar-rename-tab tabspaces-default-tab))

(provide 'init-tab-bar)
;;; init-tab-bar.el ends here
