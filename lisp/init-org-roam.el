;; init-org-roam.el --- Initialize org-roam configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; https://www.reddit.com/r/emacs/comments/hg2m5s/zettelkastenorgroamorgbrain_is_crap/
;; 

;;; Code:

;; (use-package emacsql-sqlite-builtin)

;; org-roam-capture 主要参考： /Users/yangc/src/emacs.d/ody55eus-doom-emacs.d/doom/Emacs.org
(use-package org-roam
  :delight " 𝕫"
  :custom
  ;; (org-roam-database-connector 'sqlite-builtin)
  (org-roam-directory "~/notes/09_Zettelkästen")
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head "${slug}.txt"
                         "#+TITLE: ${title}\n#+ROAM_TAGS:\n#+ROAM_KEY:\n")
      :unnarrowed t)
     ("p" "personal" plain "%?" :target
      (file+head "Personal/%<%Y%m%d%H%M%S>-${slug}.txt"
                 "#+title: ${title}\n#+ROAM_TAGS:\n#+ROAM_KEY:\n")
      :unnarrowed t)
     ("w" "work" plain "%?" :target
      (file+head "Work/%<%Y%m%d%H%M%S>-${slug}.txt"
                 "#+title: ${title}\n#+ROAM_TAGS:\n#+ROAM_KEY:\n")
      :unnarrowed t)
     ("c" "default" plain
      "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.txt"
                         "#+title: ${title}\n#+created: %U\n#+last_modified: %U\n\n")
      :unnarrowed t)
     ("r" "ref" plain
      "%?"
      :target (file+head "References/${citekey}.txt"
                         "#+title: ${title}\n#+created: %U\n#+last_modified: %U\n\n")
      :unnarrowed t)
     ("p" "ref + physical" plain
      "%?"
      :target (file+head "References/${citekey}.txt"
                         "#+title: ${title}\n#+created: %U\n#+last_modified: %U\n\n* Notes :physical:")
      :unnarrowed t)
     ("n" "ref + noter" plain
      "%?"
      :target (file+head "References/${citekey}.txt"
                         ,(s-join "\n" (list "#+title: ${title}\n#+created: %U\n#+last_modified: %U\n"
                                             "* Notes :noter:"
                                             ":PROPERTIES:"
                                             ":NOTER_DOCUMENT: %(orb-get-attached-file \"${citekey}\")"
                                             ":NOTER_PAGE:"
                                             ":END:")))
      :unnarrowed t)))
  (org-roam-capture-ref-templates
   '(("r" "ref" plain
      "%?"
      :target (file+head "web/${slug}.txt"
                         
                         :unnarrowed t)
      ("R" "Reference" plain
       "%?\n\n* Citations\n#+begin_quote\n${body}\n#+end_quote"
       :if-new (file+head
                "Literature/%<%Y%m%d%H%M%S>-${slug}.txt"
                "#+title: ${title}\n#+date: %U\n")
       :unnarrowed t
       )
      ("l" "Literature References" plain
       "%?\n\n* Abstract\n#+begin_quote\n${body}\n#+end_quote"
       :if-new (file+head
                "References/%<%Y%m%d%H%M%S>-${slug}.txt"
                "#+title: ${title}\n#+date: %U\n#+ROAM_REF: ${ref}")
       :unnarrowed t
       :empty-lines 1)
      ("w" "Web site" entry
       :target (file+head
                "Literature/%<%Y%m%d%H%M%S>-${slug}.txt"
                "#+title: ${title}\n#+date: %U\n")
       "* %a :website:\n\n%U %?\n\n#+begin_quote\n%:initial\n#+end_quote")
      ("i" "incremental" plain
       "* %?\n%(zp/org-protocol-insert-selection-dwim \"%i\")"
       :target (file+head "web/${slug}.txt"
                          "#+title: ${title}\n#+roam_key: ${ref}\n#+created: %u\n#+last_modified: %U\n\n")
       :unnarrowed t
       :empty-lines-before 1))))
  
  (org-roam-dailies-directory "daily/")
  (org-roam-dailies-capture-templates
   '(("d" "daily" entry
      "* %i%?"
      :target (file+head "daily-%<%Y-%m-%d>.txt"
                         "#+TITLE: %<%A, %d %b %Y>\n#+filetags: :daily-notes:\n\n"))
     ("l" "log entry" plain 
      "**** %<%I:%M %p> - %?"
      :if-new (file+datetree "%<%Y>.txt" :day)
      :prepend t
      :clock-in t :clock-resume t
      :empty-lines 1)
     ("m" "meeting" entry
      "**** %<%I:%M %p> - %^{Meeting Title}  :meetings:\n\n%?\n\n"
      :if-new (file+datetree "%<%Y>.txt" :day)
      :prepend t
      :clock-in t :clock-resume t
      :empty-lines 1)
     ))
  :config
  (setq org-roam-graph-extra-config '(("overlap" . "prism")
                                      ("color" . "skyblue")
                                      ;; ("color" . "#DEDEFF")
                                      ("shape" . "ellipse")
                                      ("style" . "rounded,filled")
                                      ("fillcolor" . "#EFEFFF")
                                      ("fontcolor" . "#111111")))
  (setq org-roam-file-extensions '("txt"))
  (setq org-roam-graphviz-executable "/usr/local/Cellar/graphviz/2.44.1/bin/dot")
  (setq org-roam-graphviz-extra-options '(("overlap" . "false")))
  (setq org-roam-completion-system 'ivy)
  (setq org-roam-verbose t)
  (setq org-roam-db-location
        (concat org-roam-directory "/.database/org-roam.db"))

  ;; 这里和默认的 org-mode refile 重复设置了
  ;; (setq org-refile-targets '((org-roam-list-files . (:maxlevel . 1))))

  ;; display org-roam buffer to the right
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer)))

  ;; ;; Codes blow are used to general a hierachy for title nodes that under a file
  ;; (cl-defmethod org-roam-node-doom-filetitle ((node org-roam-node))
  ;;     "Return the value of \"#+title:\" (if any) from file that NODE resides in.
  ;;     If there's no file-level title in the file, return empty string."
  ;;   (or (if (= (org-roam-node-level node) 0)
  ;;           (org-roam-node-title node)
  ;;         (org-roam-get-keyword "TITLE" (org-roam-node-file node)))
  ;;       ""))
  
  ;; (cl-defmethod org-roam-node-doom-hierarchy ((node org-roam-node))
  ;;     "Return hierarchy for NODE, constructed of its file title, OLP and direct title.
  ;;       If some elements are missing, they will be stripped out."
  ;;   (let ((title     (org-roam-node-title node))
  ;;         (olp       (org-roam-node-olp   node))
  ;;         (level     (org-roam-node-level node))
  ;;         (filetitle (org-roam-node-doom-filetitle node))
  ;;         (separator (propertize " > " 'face 'shadow)))
  ;;     (cl-case level
  ;;       ;; node is a top-level file
  ;;       (0 filetitle)
  ;;       ;; node is a level 1 heading
  ;;       (1 (concat (propertize filetitle 'face '(shadow italic))
  ;;                  separator title))
  ;;       ;; node is a heading with an arbitrary outline path
  ;;       (t (concat (propertize filetitle 'face '(shadow italic))
  ;;                  separator (propertize (string-join olp " > ") 'face '(shadow italic))
  ;;                  separator title)))))

  ;; (setq org-roam-node-display-template (concat "${type:15} ${doom-hierarchy:80} " (propertize "${tags:*}" 'face 'org-tag)))

  (org-roam-setup)
  :general
  (yc/nonprefix-keys
    "<f4>" 'org-roam-dailies-capture-today)
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

(use-package org-roam-bibtex
  :after org-roam
  :config
  (use-package org-ref)
  (setq orb-attached-file-extensions '("pdf" "epub"))
  (org-roam-bibtex-mode 1))

(use-package org-roam-protocol
  :ensure nil
  :after org-roam
  :config
  (require 'org-roam-protocol))

(use-package websocket
  :ensure t
  :after org-roam)

(use-package org-roam-ui
  :after org-roam
  :init
  (when (featurep 'xwidget-internal)
    (setq org-roam-ui-browser-function #'xwidget-webkit-browse-url))
  :hook (org-roam . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; (use-package gkroam
;;   :init
;;   (setq gkroam-root-dir "~/notes/"
;;         gkroam-pub-dir "~/notes/0x00_GTD/")
;;   :config
;;   ;; when this minor mode is on, show and hide brackets dynamically.
;;   (gkroam-dynamic-brackets-mode -1))

(provide 'init-org-roam)
