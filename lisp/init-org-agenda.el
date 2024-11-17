;; init-org-agenda.el --- Initialize org-agenda configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; - 参考：https://writequit.org/denver-emacs/presentations/2017-04-11-time-clocking-with-org.html
;; - 使用 org-starter 简化配置：https://github.com/akirak/org-starter
;; - 生成可视化的每周周报：http://amoghpj.github.io/org-report-graphics/
;; 

;;; Code:
(require 'init-org)

;; TODO: 具体配置参考这个：
;; - https://github.com/alphapapa/org-super-agenda
;; - https://github.com/alphapapa/org-super-agenda/blob/master/examples.org
(use-package org-super-agenda
  :bind
  (:map org-super-agenda-header-map
        ("<tab>" . origami-toggle-node))
  :commands (org-agenda org-super-agenda)
  :hook
  ((org-agenda-mode . org-super-agenda-mode)
   (org-super-agenda-mode . origami-mode)    ; Easily fold groups via TAB.
   (org-agenda-mode . (lambda ()
                        (setq-local line-spacing 0.8))))
  :custom-face
  (org-agenda-date ((t (:height 160))))
  (org-agenda-date-today ((t (:height 160))))
  (org-agenda-done ((t (:height 160))))
  (org-scheduled-today ((t (:height 160))))
  (org-super-agenda-header ((t (:height 160))))
  (org-scheduled ((t (:height 160))))
  (org-agenda-structure ((t (:height 160))))
  (org-agenda-dimmed-todo-face ((t (:height 160))))
  (org-time-grid ((t (:height 160))))
  (org-todo ((t (:height 160))))
  (org-done ((t (:height 160))))
  (org-imminent-deadline ((t (:height 160))))
  (org-agenda-calendar-event ((t (:height 160))))
  (org-upcoming-distant-deadline ((t (:height 160))))
  (org-upcoming-deadline ((t (:height 160))))
  (org-agenda-current-time ((t (:height 160))))
  :init
  (setq org-super-agenda-groups
        '((:log t)                      ; Automatically named "Log"
          (:name "Schedule"
                 :time-grid t)
          (:name "Delegated items"
                 :todo "DELEGATED"
                 :order 100)
          (:name "Today's items"
                 :todo ("NEXT"))
          (:name "Due today"
                 :deadline today)
          ;; (:name "Today"
          ;;  :and (:scheduled today
          ;;        :not (:todo "DELEGATED")))
          (:habit t)
          (:name "Overdue"
                 :deadline past)
          (:name "Due soon"
                 :deadline future)
          (:name "Scheduled earlier"
                 :scheduled past)
          (:name "Current Week"
                 :todo ("TODO"))
          ;; (:name "Unimportant"
          ;;        :todo ("SOMEDAY" "MABE" "CHECK" "TO-READ" "TO-WATCH"))
          )))

(use-package org-agenda
  :ensure nil
  :bind
  (("C-c a"   . org-agenda)
   ("C-c c"   . org-capture)
   ("C-c C-t" . yc/org-todo-force-notes)
   ([f6]      . (lambda ()
                  (interactive)
                  (org-capture nil "m")))
   ([f7]      . (lambda ()
                  (interactive)
                  (org-capture nil "n"))))
  :mode-hydra
  (org-agenda-mode
   (:title "Org Agenda Mode" :color pink :pre (setq which-key-inhibit t) :post (setq which-key-inhibit nil) :hint nil :separator "═")
   ("Headline"
    (("hp" org-agenda-priority "set priority")
     ("ht" org-agenda-todo "set status")
     ("hk" org-agenda-kill "kill")
     ("hr" org-agenda-refile "refile")
     ("hA" org-agenda-archive-default "archive")
     ("h:" org-agenda-set-tags "set tags"))
    "Visit entry"
    (("SPC" org-agenda-show-and-scroll-up "in other window")
     ("<tab>" org-agenda-goto "go to location" :exit t)
     ("RET" org-agenda-switch-to "enter & delete other window" :exit t)
     ("o"   link-hint-open-link "link" :exit t))
    "Date"
    (("ds" org-agenda-schedule "schedule")
     ("dS" (lambda () (interactive)
             (let ((current-prefix-arg '(4)))
               (call-interactively 'org-agenda-schedule))) "un-schedule")
     ("dd" org-agenda-deadline "set deadline")
     ("dD" (lambda () (interactive)
             (let ((current-prefix-arg '(4)))
               
               ("dt" org-agenda-date-prompt "timestamp")
               ("+" org-agenda-do-date-later "do later")
               ("-" org-agenda-do-date-earlier "do earlier")))))
    "View"
    (("vd" org-agenda-day-view "day")
     ("vw" org-agenda-week-view "week")
     ("vt" org-agenda-fortnight-view "fortnight")
     ("vm" org-agenda-month-view "month")
     ("vy" org-agenda-year-view "year")
     ("vn" org-agenda-later "next span")
     ("vp" org-agenda-earlier "prev span")
     ("vr" org-agenda-reset-view "reset"))
    "Filter"
    (("ft" org-agenda-filter-by-tag "by tag")
     ("fr" org-agenda-filter-by-tag-refine "refine by tag")
     ("fc" org-agenda-filter-by-category "by category")
     ("fh" org-agenda-filter-by-top-headline "by top headline")
     ("fx" org-agenda-filter-by-regexp "by regexp")
     ("fd" org-agenda-filter-remove-all "delete all filters"))
    "Toggle mode"
    (("tf" org-agenda-follow-mode "follow")
     ("tl" org-agenda-log-mode "log")
     ("ta" org-agenda-archives-mode "archive")
     ("tr" org-agenda-clockreport-mode "clock report")
     ("td" org-agenda-toggle-diary "diaries"))
    "Clock"
    (("cI" org-agenda-clock-in "in" :exit t)
     ("cO" org-agenda-clock-out "out")
     ("cj" org-agenda-clock-goto "goto" :exit t)
     ("cd" org-clock-display "display")
     ("ce" org-clock-modify-effort-estimate "effort")
     ("cr" org-clock-report "report")
     ("cq" org-agenda-clock-cancel "cancel"))
    "Other"
    (("gr" org-agenda-redo "reload")
     ("." org-agenda-goto-today "go to today")
     ("gd" org-agenda-goto-date "go to date"))))
  :commands org-agenda
  :config
  (require 'org-habit)
  (setq org-agenda-show-future-repeats nil
        org-agenda-start-on-weekday 1
        ;; org-agenda-inhibit-startup t ;; faster with no hidden headings (agenda performance)
        org-reverse-note-order t
        org-log-note-state 'note
        org-todo-log-states 'note
        org-agenda-skip-deadline-if-done nil
        org-agenda-skip-scheduled-if-done nil)

  ;; warn of deadlines in the next 7 days
  (setq org-deadline-warning-days 7)

  ;; don't display tags in org-agenda view
  (setq org-agenda-hide-tags-regexp ".")

  ;; show tasks in the next 14 days
  (setq org-agenda-span (quote fortnight))

  ;; org sorthing strategy
  (setq org-agenda-sorting-strategy '(time-up priority-down))

  ;; Enable sticky agenda: `q' key will bury agenda buffers (instead of killing).
  (setq org-agenda-sticky t)

  ;; don't show tasks as scheduled if they are already shown as a deadline
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)

  ;; don't give awarning colour to tasks with impending deadlines
  ;; if they are scheduled to be done
  (setq org-agenda-skip-deadline-prewarning-if-scheduled '(pre-scheduled))

  (setq org-agenda-todo-ignore-scheduled 'future)

  ;; TODO: 参考这篇 https://blog.jethro.dev/posts/processing_inbox/(file:~/src/emacs.d/jethrokuan-dots/.doom.d/config.el、~/src/emacs.d/Kinneyzhang-emacs.d/elisp/init-org.el::jethro/org-process-inbox)，设置 refile 的流程步骤
  ;; 这里的思路：类似于其他的 GTD 软件，在整理 Inbox 的 todo 项时，要设置比如 tags、预计时间、项目等模板化信息，在 orgmode 中的思路则是通过调用一个组合的 function，依次调用相应的命令。

  ;; http://cachestocaches.com/2016/9/my-workflow-org-agenda/#capture--refile
  (setq org-directory "~/notes/Zettelkästen/")
  (setq org-agenda-file-inbox (expand-file-name "0x00_GTD/00_inbox.txt" org-directory))
  (setq org-agenda-file-gtd (expand-file-name "0x00_GTD/01_gtd.txt" org-directory))
  (setq org-agenda-file-tickler (expand-file-name "0x00_GTD/02_tickler.txt" org-directory))
  (setq org-agenda-file-someday (expand-file-name "0x00_GTD/03_someday.txt" org-directory))
  (setq org-agenda-file-beorg (expand-file-name "0x00_GTD/04_beorg-refile.txt" org-directory))
  (setq org-agenda-file-agenda (expand-file-name "0x00_GTD/99_agenda.txt" org-directory))
  (setq org-agenda-file-done (expand-file-name "0x00_GTD/done.org_archive" org-directory))
  (setq org-agenda-file-journal (expand-file-name "journal.txt" org-directory))
  (setq org-agenda-file-thoughts (expand-file-name "thoughts.txt" org-directory))
  (setq org-agenda-file-notes (expand-file-name "0x00_GTD/notes.txt" org-directory))
  (setq org-agenda-file-code-snippet (expand-file-name "snippet.txt" org-directory))
  (setq org-agenda-diary-file (expand-file-name "diary.txt" org-directory))
  (setq org-default-notes-file (expand-file-name "0x00_GTD/00_inbox.txt" org-directory))
  ;; (setq org-agenda-file-reading (expand-file-name "0x00_GTD/05_reading.org" org-directory))
  ;; (setq org-agenda-files (list (concat org-directory "/0x00_GTD")))

  (setq org-agenda-files (apply 'append
                                (mapcar
                                 (lambda (directory)
                                   (directory-files-recursively
                                    directory "\\.org$\\|\\.txt$\\|\\.org_archive$"))
                                 '("~/notes/Zettelkästen/0x00_GTD/" "~/notes/Zettelkästen/daily/"))))

  (setq org-refile-targets '((org-agenda-file-inbox   :maxlevel . 1)
                             (org-agenda-file-gtd     :maxlevel . 2)
                             (org-agenda-file-someday :level .    1)
                             (org-agenda-file-tickler :maxlevel . 2)))

  (setq org-outline-path-complete-in-steps nil)    ; refile in a single go
  (setq org-refile-use-outline-path t)             ; show full paths for refiling

  ;; Eisenhower Matrix
  (setq org-lowest-priority ?E)
  (setq org-default-priority ?E)
  (setq org-agenda-sticky t)  ;generate differens agendas buffers in separated windows
  
  ;; - https://ivanmalison.github.io/dotfiles/
  ;; - https://emacs.stackexchange.com/questions/90/how-to-sometimes-but-not-always-add-a-note-to-an-org-todo-state-change
  (defun yc/org-todo-force-notes ()
    (interactive)
    (let ((org-todo-log-states
           (mapcar (lambda (state)
                     (list state 'note 'time))
                   (apply 'append org-todo-sets))))
      (call-interactively 'org-todo)))

  ;; 使用 =C-c C-x C-c= 进入 org-columns 模式，可以看到任务总结
  (setq org-columns-default-format "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)")
  ;; Set default column view headings: Task Priority Effort Clock_Summary
  ;; (setq org-columns-default-format "%50ITEM(Task) %2PRIORITY %10Effort(Effort){:} %10CLOCKSUM")
  ;; (setq org-columns-default-format "%50ITEM(Task) %20CLOCKSUM %16TIMESTAMP_IA")
  ;; (setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")

  ;; Formats the agenda into nice columns
  (setq org-agenda-prefix-format
        '((agenda . " %i %-12:c%?-12t% s")
          (todo   . " ")
          (tags   . " %i %-12:c")
          (search . " %i %-12:c")))

  ;; - https://emacs.stackexchange.com/questions/16551/how-do-i-view-all-org-mode-todos-that-are-not-recurring-or-not-scheduled
  (setq org-agenda-custom-commands
        '(
          ("c" "Unscheduled TODO"
           ((todo ""
                  ((org-agenda-overriding-header "\nUnscheduled TODO")
                   (org-agenda-skip-function '(org-agenda-skip-if nil '(scheduled deadline))))))
           nil
           nil)
          ("d" "Daily Tasks"
           alltodo "" ((org-super-agenda-groups
                        '((:log t)
                          (:name "Delegated items"
                                 :todo "DELEGATED"
                                 :order 100)
                          (:name "Today's Tasks"
                                 :and (:scheduled today
                                                  :not (:todo "DELEGATED")))
                          (:discard (:anything t)))
                        )))
          ("g" "Get Things Done (GTD)"
           ((agenda ""
                    ((org-agenda-skip-function
                      '(or
                        (org-agenda-skip-entry-if 'deadline)
                        (org-agenda-skip-entry-if 'todo '("DELEGATED"))
                        ))
                     (org-deadline-warning-days 0)))
            (todo "NEXT"
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline))
                   (org-agenda-prefix-format "  %i %-12:c [%e] ")
                   (org-agenda-overriding-header "\nTasks\n")))
            (todo "DELEGATED"
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline))
                   (org-agenda-prefix-format "  %i %-12:c [%e] ")
                   (org-agenda-overriding-header "\nDelegated items\n")))
            ;; (agenda nil
            ;;         ((org-agenda-entry-types '(:deadline))
            ;;          (org-agenda-format-date "")
            ;;          (org-deadline-warning-days 7)
            ;;          (org-agenda-skip-function
            ;;           '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
            ;;          (org-agenda-overriding-header "\nDeadlines")))
            (tags-todo "inbox"
                       ((org-agenda-prefix-format "  %?-12t% s")
                        (org-agenda-overriding-header "\nInbox\n")))
            (tags "CLOSED>=\"<today>\""
                  ((org-agenda-overriding-header "\nCompleted today\n")))))
          ;; ("w" "WORK"
          ;;  ((agenda "" ((org-agenda-ndays 14)
          ;;               (org-agenda-start-on-weekday nil)
          ;;               (org-agenda-prefix-format " %-12:c%?-12t% s")))
          ;;   (tags-todo "yundao")
          ;;   (tags-todo "clouddesktop")
          ;;   (tags-todo "@work")))

          ;; ("S" "Super view"
          ;;  ((agenda "" ((org-super-agenda-groups
          ;;                '((:name "Today"
          ;;                         :time-grid t)))))
          ;;   (todo "" ((org-agenda-overriding-header "Projects")
          ;;             (org-super-agenda-groups
          ;;              '((:name none  ; Disable super group header
          ;;                       :children todo)
          ;;                (:discard (:anything t))))))))

          ;; ("p" "personal"
          ;;  ((agenda)
          ;;   (tags-todo "perso")))
          ;; ("u" "UDAL 安排给其他人的任务"
          ;;  ((agenda "" ((org-agenda-ndays 14)
          ;;               (org-agenda-start-on-weekday nil)
          ;;               (org-agenda-prefix-format " %-12:c%?-12t% s")))
          ;;   (tags-todo "刘志斌\|房燕文\|骆建斌")))

          ;; ("W" "Weekly Overview"
          ;;  todo "" ((org-super-agenda-groups
          ;;            '((:name "This Week's Tasks"
          ;;                     :todo "NEXT")
          ;;              (:name "Delayed Tasks"
          ;;                     :todo "DELAYED")
          ;;              (:name "In Progress"
          ;;                     :todo "STARTED")
          ;;              (:discard (:anything))))))

          ;; ("z" "Super view" (
          ;;                    (agenda "" ((org-agenda-span 'day)
          ;;                                (org-super-agenda-groups
          ;;                                 '((:name "Today"
          ;;                                          :time-grid t
          ;;                                          :date today
          ;;                                          :todo "TODAY"
          ;;                                          :scheduled today
          ;;                                          :order 1
          ;;                                          )))))
          ;;                    (alltodo "" ((org-agenda-overriding-header "")
          ;;                                 (org-super-agenda-groups
          ;;                                  '((:name "Next to do"
          ;;                                           :todo "NEXT"
          ;;                                           :order 1)
          ;;                                    (:name "Important"
          ;;                                           :tag "Important"
          ;;                                           :priority "A"
          ;;                                           :order 6)
          ;;                                    (:name "Due Today"
          ;;                                           :deadline today
          ;;                                           :order 2)
          ;;                                    (:name "Due Soon"
          ;;                                           :deadline future
          ;;                                           :order 8)
          ;;                                    (:name "Overdue"
          ;;                                           :deadline past
          ;;                                           :order 7)
          ;;                                    (:name "Assignments"
          ;;                                           :tag "Assignment"
          ;;                                           :order 10)
          ;;                                    (:name "Issues"
          ;;                                           :tag "Issue"
          ;;                                           :order 12)
          ;;                                    (:name "Projects"
          ;;                                           :tag "Project"
          ;;                                           :order 14)
          ;;                                    (:name "Work"
          ;;                                           :tag "@work"
          ;;                                           :order 3)
          ;;                                    (:name "Emacs"
          ;;                                           :tag "EMACS"
          ;;                                           :order 13)
          ;;                                    (:name "macOS"
          ;;                                           :tag "macOS"
          ;;                                           :order 14)
          ;;                                    (:name "Research"
          ;;                                           :tag "Research"
          ;;                                           :order 15)
          ;;                                    (:name "To read"
          ;;                                           :tag "Read"
          ;;                                           :order 30)
          ;;                                    (:name "Waiting"
          ;;                                           :todo "WAITING"
          ;;                                           :order 20)
          ;;                                    (:name "trivial"
          ;;                                           :priority<= "C"
          ;;                                           :tag ("Trivial" "Unimportant")
          ;;                                           :todo ("SOMEDAY" )
          ;;                                           :order 90)
          ;;                                    (:discard (:tag ("Chore" "Routine" "Daily")))
          ;;                                    ))
          ;;                                 )))
          ;;  )
          ;; ("r" "Reading"
          ;;  ((tags-todo "CATEGORY=\"Reading\""
          ;;              ((org-agenda-prefix-format "%:T ")))))
          ;; ("m" "Movies"
          ;;  ((tags-todo "CATEGORY=\"Movies\""
          ;;              ((org-agenda-prefix-format "%:T ")))))


          ;; 参考设置： https://www.reddit.com/r/emacs/comments/fxb492/updated_my_literate_doom_config_sharing_config/
          ;; ("k" "Tasks"
          ;;    ((agenda ""
          ;;             ((org-agenda-overriding-header "Agenda")
          ;;              (org-agenda-span 'day)
          ;;              (org-agenda-start-day (org-today))
          ;;              ))
          ;;     (todo ""
          ;;           ((org-agenda-overriding-header "Tasks")
          ;;            (org-agenda-skip-function
          ;;             '(or
          ;;               (and
          ;;                (org-agenda-skip-entry-if 'notregexp "#[A-C]")
          ;;                (org-agenda-skip-entry-if 'notregexp ":@\\w+"))
          ;;               (org-agenda-skip-if nil '(scheduled deadline))
          ;;               (org-agenda-skip-if 'todo '("SOMEDAY"))))
          ;;            (org-super-agenda-groups
          ;;             '((:name "Priority Items"
          ;;                      :priority>= "B")
          ;;               (:auto-parent t)))))
          ;;     (todo ""
          ;;           ((org-agenda-overriding-header "Delegated Tasks")
          ;;            (org-tags-match-list-sublevels t)
          ;;            (org-agenda-skip-function
          ;;             '(or
          ;;               (org-agenda-skip-subtree-if 'nottodo '("DELEGATED"))))
          ;;            (org-super-agenda-groups
          ;;             '((:auto-property "WHO")))))))
          ;;   ("n" "Notes"
          ;;    ((todo ""
          ;;           ((org-agenda-overriding-header "Note Actions")
          ;;            (org-super-agenda-groups
          ;;             '((:auto-category t)))))))
          ;; ("i" "Inbox"
          ;;  ((todo ""
          ;;         ((org-agenda-overriding-header "Inbox")
          ;;          (org-agenda-skip-function
          ;;           '(or
          ;;             (org-agenda-skip-entry-if 'regexp ":@\\w+")
          ;;             (org-agenda-skip-entry-if 'regexp "\[#[A-E]\]")
          ;;             (org-agenda-skip-if 'nil '(scheduled deadline))
          ;;             (org-agenda-skip-entry-if 'todo '("SOMEDAY"))
          ;;             (org-agenda-skip-entry-if 'todo '("DELEGATED"))))
          ;;          (org-super-agenda-groups
          ;;           '((:auto-ts t)))))))
          ;; ("s" "Someday"
          ;;  ((todo ""
          ;;         ((org-agenda-overriding-header "Someday")
          ;;          (org-agenda-skip-function
          ;;           '(or
          ;;             (org-agenda-skip-entry-if 'nottodo '("SOMEDAY"))))
          ;;          (org-super-agenda-groups
          ;;           '((:auto-parent t)))))))
          ))

  ;; @see - https://github.com/bastibe/org-journal#journal-capture-template
  (defun org-journal-find-location ()
    ;; Open today's journal, but specify a non-nil prefix argument in order to
    ;; inhibit inserting the heading; org-capture will insert the heading.
    
    ;; Position point on the journal's top-level heading so that org-capture
    ;; will add the new entry as a child entry.
    (goto-char (point-min)))

  ;; 使用弹出一个 frame 方式打开 org-capture。
  ;; - %u -- 插入当前日志[2017-07-17 Mon]
  ;; - %U -- 插入当前日志，并有具体时间[2017-07-17 Mon 16:48]
  ;; - %T -- 时间格式不同而已<2017-07-17 Mon 16:48>
  ;; - %a -- 插入当前所在文档的 link 地址

  ;; - https://www.reddit.com/r/emacs/comments/7zqc7b/share_your_org_capture_templates/
  ;; - https://github.com/sprig/org-capture-extension

  ;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
  ;; :empty-lines 2
  (setq org-capture-templates
        '(
          ;; ("t" "todo [inbox]" entry (file+headline org-default-notes-file "Inbox")
          ;;  "* TODO %i%? %^g\nSCHEDULED: %t\n%T\n" :clock-in t :clock-resume t :prepend t :empty-lines 1)
          ("d" "todo [inbox]" entry (file+headline org-default-notes-file "Inbox")
           "** TODO %? %^g\n:PROPERTIES:\n:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:END:\n:LOGBOOK:\n- CREATED: %U\n:END:\n_Desired outcome:_ %^{Desired outcome:}\n\n- ~工作事项~ [0%] ::\n  - [ ] \n" :clock-in t :clock-resume t :prepend t :empty-lines 1)
          ("m" "Meeting" entry (file+headline org-default-notes-file "Inbox")   ; Meeting
           "* MEETING 会议主题：%? :MEETING:\n%T\n\n*与会人员*\n- \n\n*会议议题*\n\n*重点结论*\n\n*会后 TODO*\n\n*其他事项*\n" :clock-in t :clock-resume t :prepend t :empty-lines 1)
          ("w" "Work TODO" entry (file+olp org-default-notes-file "Work" "Tasks")
           "* TODO %? :work:\n:LOGBOOK:\n- CREATED: %U\n:END:" :clock-in t :clock-resume t)
          ("a" "Appointment" entry (file+headline org-default-notes-file "Appointments")
           "* TODO %?\n:PROPERTIES:\n\n:END:\nDEADLINE: %^T \n %i\n")
          ("l" "Link" entry (file+headline org-agenda-file-gtd "Reading List")
           "* READ %^{Description}\n%u\n\n%c\n\n%i" :prepend t :empty-lines 1)
          ("L" "A link, for reading later." entry (file+headline org-agenda-file-gtd "Reading List")
           "* READ %?%^L %^g\n%T" :prepend t :empty-lines 1)
          ("K" "Cliplink capture task" entry (file+headline org-agenda-file-gtd "Reading List")
           "* READ %(org-cliplink-capture)\n" :prepend t :empty-lines 1)
          ("n" "Note" entry (file org-agenda-file-notes)
           "* Note %?\n%T")
          ("N" "NOTES" entry (file org-agenda-file-notes)
           "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
          ("i" "Idea" entry (file+headline org-agenda-file-thoughts "Thoughts")
           "* %? :IDEA:\n%t" :clock-in t :clock-resume t)
          ("b" "Blog idea" entry (file+headline org-default-notes-file "Blog Topics:")
           "* %?\n%T" :prepend t)
          ;;  "* %?\nEntered on %U\n  %i\n  %a")
          ("j" "Journal")
          ("jd" "Diary" entry (file+olp+datetree "diary.txt")
           "* %?\n%U\n" :clock-in t :clock-resume t)
          ("jj" "Journal entry" entry (function org-journal-find-location)
           "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")
          ("jo" "Journal" entry  (file (concat org-directory "/refile.txt"))
           "* %?\n%U\n" :clock-in t :clock-resume t)
          ("s" "Screencast" entry (file org-default-notes-file)
           "* %?\n%i\n")
          ("r" "RESPONED" entry  (file (concat org-directory "/refile.txt"))
           "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
          ("W" "org-protocol" entry  (file (concat org-directory "/refile.txt"))
           "* TODO Review %c\n%U\n" :immediate-finish t)
          ("p" "Phone call" entry  (file (concat org-directory "/refile.txt"))
           "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
          ("T" "Tickler" entry (file+headline org-agenda-file-tickler "Tickler")
           "* %i%?\n %U")
          ("h" "Habit" entry  (file (concat org-directory "/refile.txt"))
           "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
          ))

  ;; Agendas should be full screen!
  ;; (add-hook 'org-agenda-finalize-hook (lambda () (delete-other-windows)))
  
  ;; ;; This function opens the agenda in full screen.
  ;; (defun yc/open-agenda ()
  ;;   "Opens the org-agenda."
  ;;   (interactive)
  ;;   (let ((agenda "*Org Agenda*"))
  ;;     (if (equal (get-buffer agenda) nil)
  ;;         (org-agenda-list)
  ;;       (unless (equal (buffer-name (current-buffer)) agenda)
  ;;         (switch-to-buffer agenda))
  ;;       (org-agenda-redo t)
  ;;       (beginning-of-buffer))))

  ;; Archive subtrees under the same hierarchy as original in the archive files
  ;; 参考：https://github.com/Fuco1/Fuco1.github.io/blob/master/posts/2017-04-20-Archive-subtrees-under-the-same-hierarchy-as-original-in-the-archive-files.org
  ;; (defadvice org-archive-subtree (around fix-hierarchy activate)
  ;;   (let* ((fix-archive-p (and (not current-prefix-arg)
  ;;                              (not (use-region-p))))
  ;;          (afile (org-extract-archive-file (org-get-local-archive-location)))
  ;;          (buffer (or (find-buffer-visiting afile) (find-file-noselect afile))))
  ;;     ad-do-it
  ;;     (when fix-archive-p
  ;;       (with-current-buffer buffer
  ;;         (goto-char (point-max))
  ;;         (while (org-up-heading-safe))
  ;;         (let* ((olpath (org-entry-get (point) "ARCHIVE_OLPATH"))
  ;;                (path (and olpath (split-string olpath "/")))
  ;;                (level 1)
  ;;                tree-text)
  ;;           (when olpath
  ;;             (org-mark-subtree)
  ;;             (setq tree-text (buffer-substring (region-beginning) (region-end)))
  ;;             (let (this-command) (org-cut-subtree))
  ;;             (goto-char (point-min))
  ;;             (save-restriction
  ;;               (widen)
  ;;               (-each path
  ;;                 (lambda (heading)
  ;;                   (if (re-search-forward
  ;;                        (rx-to-string
  ;;                         `(: bol (repeat ,level "*") (1+ " ") ,heading)) nil t)
  ;;                       (org-narrow-to-subtree)
  ;;                     (goto-char (point-max))
  ;;                     (unless (looking-at "^")
  ;;                       (insert "\n"))
  ;;                     (insert (make-string level ?*)
  ;;                             " "
  ;;                             heading
  ;;                             "\n"))
  ;;                   (cl-incf level)))
  ;;               (widen)
  ;;               (org-end-of-subtree t t)
  ;;               (org-paste-subtree level tree-text))))))))

  ;; 参考这个配置自动归档：https://emacs.stackexchange.com/questions/19995/automatically-archive-done-entries-regardless-of-keyword
  ;; https://stackoverflow.com/questions/6997387/how-to-archive-all-the-done-tasks-using-a-single-command
  ;; (defun yc/org-archive-done-tasks ()
  ;;   (interactive)
  ;;   (org-map-entries 'org-archive-subtree "/DONE" 'file))

  ;; (defun yc/org-buffer-day-agenda ()
  ;;   (interactive)
  ;;   "Creates an agenda for the current buffer. Equivalent to the sequence: org-agenda, < (restrict to current buffer), a (agenda-list), d (org-agenda-day-view)."
  ;;   (progn
  ;;     (org-agenda-set-restriction-lock 'file)
  ;;     (org-agenda-list)
  ;;     (org-agenda-day-view))) ;; Maybe I should try writing a Emacs Lisp macro for this kind of thing!
  )

;; - https://github.com/alphapapa/org-ql
;; org-ql 常用命令的使用说明 https://emacs-china.org/t/org-ql/21486/2
(use-package org-ql
  :commands (org-ql-view org-ql-search))

;; Pomodoro
(use-package org-pomodoro
  :after org org-agenda
  :bind
  (:map org-agenda-mode-map
        ("P" . org-pomodoro)))

;; Add graphical view of agenda
;; (use-package org-timeline
;;   :hook (org-agenda-finalize . org-timeline-insert-timeline))

(defun yc/find-all-org-files-in-notes (&optional rec)
  "Find files with TXT in notes.
REC searches recursively."
  (interactive)
  (split-string (shell-command-to-string (format "fd . '%s' -e %s" "/Users/yangc/notes" "txt"))))  ; find %s -not -path '*/\.*' -name '*.%s'

(provide 'init-org-agenda)
;;; init-org-agenda.el ends here
