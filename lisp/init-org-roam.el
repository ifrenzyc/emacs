;; init-org-roam.el --- Initialize org-roam configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; https://www.reddit.com/r/emacs/comments/hg2m5s/zettelkastenorgroamorgbrain_is_crap/
;; 

;;; Code:

(use-package org-roam
  ;; :straight (:host github :repo "org-roam/org-roam")
  :init
  ;; disable v1 migrate to v2 warning
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/notes/09_Zettelkästen")
  :config
  (setq org-roam-file-extensions '("txt"))
  (setq org-roam-graphviz-executable "/usr/local/Cellar/graphviz/2.44.1/bin/dot")
  (setq org-roam-graphviz-extra-options '(("overlap" . "false")))
  (setq org-roam-completion-system 'ivy)
  (setq org-roam-verbose t)
  (setq org-roam-db-location
        (concat org-roam-directory "/.database/org-roam.db"))
  (org-roam-setup)
  :general
  (yc/leader-keys
      "ar" '(:ignore t :which-key "org-roam")
    "arl" 'org-roam-buffer-toggle
    "art" 'org-roam-dailies-today
    "arf" 'org-roam-node-find
    "arg" 'org-roam-show-graph)
  (org-roam-mode-map
   "C-c n l" 'org-roam-buffer-toggle
   "C-c n f" 'org-roam-node-find
   "C-c n g" 'org-roam-graph)
  (org-mode-map
   "C-c n i" 'org-roam-node-insert
   "C-c n I" 'org-roam-insert-immediate)
  ;; (yc/leader-keys-major-mode
  ;;   :keymaps 'org-mode-map
  ;;   "r"  '(:ignore t :which-key "org-roam")
  ;;   "rl" 'org-roam
  ;;   "rt" 'org-roam-dailies-today
  ;;   "rb" 'org-roam-switch-to-buffer
  ;;   "rf" 'org-roam-find-file
  ;;   "ri" 'org-roam-insert
  ;;   "rg" 'org-roam-show-graph)
  )

;; https://github.com/org-roam/org-roam-server
(use-package org-roam-server)

(use-package org-roam-ui
  :straight
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  :hook (org-roam . org-roam-ui-mode))

;; (use-package gkroam
;;   :init
;;   (setq gkroam-root-dir "~/notes/"
;;         gkroam-pub-dir "~/notes/0x00_GTD/")
;;   :config
;;   ;; when this minor mode is on, show and hide brackets dynamically.
;;   (gkroam-dynamic-brackets-mode -1))

(provide 'init-org-roam)
