;; init-custom.el --- Initialize custom settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:

;; Personal Information
(setq user-full-name "Yang Chuang")
(setq user-mail-address "ifrenzyc@gmail.com")


;; TODO: 参考这篇 https://blog.jethro.dev/posts/processing_inbox/(file:~/src/emacs.d/jethrokuan-dots/.doom.d/config.el、~/src/emacs.d/Kinneyzhang-emacs.d/elisp/init-org.el::jethro/org-process-inbox)，设置 refile 的流程步骤
;; 这里的思路：类似于其他的 GTD 软件，在整理 Inbox 的 todo 项时，要设置比如 tags、预计时间、项目等模板化信息，在 orgmode 中的思路则是通过调用一个组合的 function，依次调用相应的命令。

;; http://cachestocaches.com/2016/9/my-workflow-org-agenda/#capture--refile
(setq org-directory "~/notes/Zettelkästen/")
(setq org-agenda-file-inbox (expand-file-name "0x00_GTD/00_inbox.txt" org-directory))
(setq org-agenda-file-notes (expand-file-name "0x00_GTD/00_notes.txt" org-directory))
(setq org-agenda-file-work (expand-file-name "0x00_GTD/01_Work.txt" org-directory))
(setq org-agenda-file-personal (expand-file-name "0x00_GTD/01_Personal.txt" org-directory))
(setq org-agenda-file-tickler (expand-file-name "0x00_GTD/02_tickler.txt" org-directory))
(setq org-agenda-file-beorg (expand-file-name "0x00_GTD/03_beorg-refile.txt" org-directory))
(setq org-agenda-file-agenda (expand-file-name "0x00_GTD/99_agenda.txt" org-directory))
(setq org-agenda-file-done (expand-file-name "0x00_GTD/done.org_archive" org-directory))
;; (setq org-agenda-file-journal (expand-file-name "journal.txt" org-directory))
;; (setq org-agenda-file-thoughts (expand-file-name "thoughts.txt" org-directory))
;; (setq org-agenda-file-code-snippet (expand-file-name "snippet.txt" org-directory))
;; (setq org-agenda-diary-file (expand-file-name "diary.txt" org-directory))
(setq org-default-notes-file (expand-file-name "0x00_GTD/00_inbox.txt" org-directory))
;; (setq org-agenda-file-reading (expand-file-name "0x00_GTD/05_reading.org" org-directory))
;; (setq org-agenda-files (list (concat org-directory "/0x00_GTD")))


(provide 'init-custom)
;;; init-custom.el ends here
