;; init-org-journal.el --- Initialize org-journal configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 使用 org-mode 编写个人日记。
;; - https://github.com/bastibe/org-journal

;; *常用按键：*
;; | 按键    | 作用                 |
;; |---------+----------------------|
;; | =C-c j= | 创建一个新的日记条目 |

;; 关于为什么使用 =:custom= ，参考 https://github.com/bastibe/org-journal/issues/126
;; 

;;; Code

(use-package org-journal
  :disabled t
  :general
  ("C-c j" 'org-journal-new-entry)
  ;; (yc/leader-keys
  ;;   "jo" 'org-journal-new-entry)
  :custom
  (org-journal-file-type 'monthly)
  (org-journal-dir "~/notes/07_Journal/")
  (org-journal-date-format "%A, %d/%m/%Y")
  (org-journal-file-format "%Y-%m.txt")
  (org-journal-carryover-items ""))

;; (use-package org-noter
;;   :commands org-noter
;;   ;; :init
;;   ;; (yc/leader-keys-major-mode
;;   ;;   :keymaps 'pdf-view-mode-map
;;   ;;   "n" 'org-noter
;;   ;;   "i" 'org-noter-insert-note
;;   ;;   "k" 'org-noter-kill-session)
;;   ;; (yc/leader-keys-major-mode
;;   ;;   :keymaps 'org-mode-map
;;   ;;   "n" 'org-noter)
;;   :config
;;   (progn
;;     (setq org-noter-default-notes-file-names '("ref-notes.org")
;; 	      ;; org-noter-notes-search-path '("~/Dropbox/document/org/references")
;; 	      org-noter-auto-save-last-location t
;; 	      org-noter-doc-split-fraction '(0.7 . 0.7)
;; 	      org-noter-always-create-frame nil
;; 	      org-noter-insert-note-no-questions t
;; 	      ;; org-noter-notes-window-location 'vertical-split
;; 	      org-noter-notes-window-location 'horizontal-split
;; 	      org-noter-open-note-file-after-kill-session t)))

;; (use-package org-brain
;;   :init
;;   (setq org-brain-path org-directory)
;;   ;; For Evil users
;;   (with-eval-after-load 'evil
;;     (evil-set-initial-state 'org-brain-visualize-mode 'emacs))
;;   :config
;;   (setq org-id-track-globally t)
;;   (setq org-id-locations-file (expand-file-name ".org-id-locations" user-emacs-directory))
;;   (push '("b" "Brain" plain (function org-brain-goto-end)
;;           "* %i%?" :empty-lines 1)
;;         org-capture-templates)
;;   (setq org-brain-visualize-default-choices 'all)
;;   (setq org-brain-title-max-length 12)
;;   (setq org-brain-include-file-entries nil
;;         org-brain-file-entries-use-title nil))

(provide 'init-org-journal)
