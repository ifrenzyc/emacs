;; init-org-agenda.el --- Initialize org-agenda configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; - 参考：https://writequit.org/denver-emacs/presentations/2017-04-11-time-clocking-with-org.html
;; - 使用 org-starter 简化配置：https://github.com/akirak/org-starter
;; - 生成可视化的每周周报：http://amoghpj.github.io/org-report-graphics/
;; 

;;; Code:

(require 'init-org)

(use-package org-agenda
  :ensure nil
  :commands org-agenda
  :general
  ("C-c a" 'org-agenda
   "C-c c" 'org-capture)
  :config
  (require 'org-habit)
  (setq org-agenda-show-future-repeats nil
        org-agenda-start-on-weekday nil
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
  (setq org-directory "~/notes/09_Zettelkästen/")
  (setq org-agenda-file-inbox (expand-file-name "0x00_GTD/00_inbox.txt" org-directory))
  (setq org-agenda-file-gtd (expand-file-name "0x00_GTD/01_gtd.txt" org-directory))
  (setq org-agenda-file-tickler (expand-file-name "0x00_GTD/02_tickler.txt" org-directory))
  (setq org-agenda-file-someday (expand-file-name "0x00_GTD/03_someday.txt" org-directory))
  (setq org-agenda-file-beorg (expand-file-name "0x00_GTD/04_beorg-refile.txt" org-directory))
  (setq org-agenda-file-agenda (expand-file-name "0x00_GTD/99_agenda.txt" org-directory))
  (setq org-agenda-file-done (expand-file-name "0x00_GTD/done.org_archive" org-directory))
  (setq org-agenda-file-journal (expand-file-name "journal.txt" org-directory))
  (setq org-agenda-file-thoughts (expand-file-name "thoughts.txt" org-directory))
  (setq org-agenda-file-notes (expand-file-name "notes.txt" org-directory))
  (setq org-agenda-file-code-snippet (expand-file-name "snippet.txt" org-directory))
  (setq org-agenda-diary-file (expand-file-name "diary.txt" org-directory))
  (setq org-default-notes-file (expand-file-name "0x00_GTD/00_inbox.txt" org-directory))
  ;; (setq org-agenda-file-reading (expand-file-name "0x00_GTD/05_reading.org" org-directory))
  ;; (setq org-agenda-files (list (concat org-directory "/0x00_GTD")))

  (setq org-agenda-files (apply 'append
                                (mapcar
                                 (lambda (directory)
                                   (directory-files-recursively
                                    directory "\\.org$\\|\\.txt$"))
                                 '("~/notes/09_Zettelkästen/0x00_GTD/"))))

  (setq org-refile-targets '((org-agenda-file-inbox   :maxlevel . 1)
                             (org-agenda-file-gtd     :maxlevel . 2)
                             (org-agenda-file-someday :level .    1)
                             (org-agenda-file-tickler :maxlevel . 2)))

  (setq org-outline-path-complete-in-steps nil)    ; refile in a single go
  (setq org-refile-use-outline-path t)             ; show full paths for refiling

  ;; - https://ivanmalison.github.io/dotfiles/
  ;; - https://emacs.stackexchange.com/questions/90/how-to-sometimes-but-not-always-add-a-note-to-an-org-todo-state-change
  (defun yc/org-todo-force-notes ()
    (interactive)
    (let ((org-todo-log-states
           (mapcar (lambda (state)
                     (list state 'note 'time))
                   (apply 'append org-todo-sets))))
      (call-interactively 'org-todo)))

  (define-key org-mode-map (kbd "C-c C-t") 'yc/org-todo-force-notes)

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
  ;; (setq org-agenda-prefix-format
  ;;       '((agenda . " %i %-12t% s %-12(car (last (org-get-outline-path)))")
  ;;         (timeline . "  % s")
  ;;         (todo . " %i %-12:c")
  ;;         (tags . " %i %-12:c")
  ;;         (search . " %i %-12:c")))

  ;; (setq org-agenda-prefix-format
  ;;       '((agenda  . " %i %-12:c%?-12t% s")
  ;;         (timeline  . "  % s")
  ;;         (todo  . " %i %-30:c")
  ;;         (tags  . " %i %-40:c")
  ;;         (search . " %i %-12:c")))

  ;; (setq org-agenda-prefix-format
  ;;       '((agenda . " %i %-14:c%?-12t% s%5e")
  ;;         (timeline . "  % s")
  ;;         (todo . " %i %-14:c")
  ;;         (tags . " %i %-14:c")
  ;;         (search . " %i %-14:c")))

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

  ;; - https://gist.github.com/amirrajan/301e74dc844a4c9ffc3830dc4268f177
  ;; (eval-after-load 'evil
  ;;   '(progn
  ;;      (evil-set-initial-state 'org-agenda-mode 'emacs)
  ;;      (evil-define-key 'normal org-agenda-mode-map
  ;;        (kbd "<RET>") 'org-agenda-switch-to
  ;;        (kbd "\t") 'org-agenda-goto

  ;;        "q" 'org-agenda-quit
  ;;        "r" 'org-agenda-redo
  ;;        "S" 'org-save-all-org-buffers
  ;;        "gj" 'org-agenda-goto-date
  ;;        "gJ" 'org-agenda-clock-goto
  ;;        "gm" 'org-agenda-bulk-mark
  ;;        "go" 'org-agenda-open-link
  ;;        "s" 'org-agenda-schedule
  ;;        "+" 'org-agenda-priority-up
  ;;        "," 'org-agenda-priority
  ;;        "-" 'org-agenda-priority-down
  ;;        "y" 'org-agenda-todo-yesterday
  ;;        "n" 'org-agenda-add-note
  ;;        "t" 'org-agenda-todo
  ;;        ":" 'org-agenda-set-tags
  ;;        ";" 'org-timer-set-timer
  ;;        "I" 'helm-org-task-file-headings
  ;;        "i" 'org-agenda-clock-in-avy
  ;;        "O" 'org-agenda-clock-out-avy
  ;;        "u" 'org-agenda-bulk-unmark
  ;;        "x" 'org-agenda-exit
  ;;        "j"  'org-agenda-next-line
  ;;        "k"  'org-agenda-previous-line
  ;;        "vt" 'org-agenda-toggle-time-grid
  ;;        "va" 'org-agenda-archives-mode
  ;;        "vw" 'org-agenda-week-view
  ;;        "vl" 'org-agenda-log-mode
  ;;        "vd" 'org-agenda-day-view
  ;;        "vc" 'org-agenda-show-clocking-issues
  ;;        "g/" 'org-agenda-filter-by-tag
  ;;        "o" 'delete-other-windows
  ;;        "gh" 'org-agenda-holiday
  ;;        "gv" 'org-agenda-view-mode-dispatch
  ;;        "f" 'org-agenda-later
  ;;        "b" 'org-agenda-earlier
  ;;        "c" 'helm-org-capture-templates
  ;;        "e" 'org-agenda-set-effort
  ;;        "n" nil  ; evil-search-next
  ;;        "{" 'org-agenda-manipulate-query-add-re
  ;;        "}" 'org-agenda-manipulate-query-subtract-re
  ;;        "A" 'org-agenda-toggle-archive-tag
  ;;        "." 'org-agenda-goto-today
  ;;        "0" 'evil-digit-argument-or-evil-beginning-of-line
  ;;        "<" 'org-agenda-filter-by-category
  ;;        ">" 'org-agenda-date-prompt
  ;;        "F" 'org-agenda-follow-mode
  ;;        "D" 'org-agenda-deadline
  ;;        "H" 'org-agenda-holidays
  ;;        "J" 'org-agenda-next-date-line
  ;;        "K" 'org-agenda-previous-date-line
  ;;        "L" 'org-agenda-recenter
  ;;        "P" 'org-agenda-show-priority
  ;;        "R" 'org-agenda-clockreport-mode
  ;;        "Z" 'org-agenda-sunrise-sunset
  ;;        "T" 'org-agenda-show-tags
  ;;        "X" 'org-agenda-clock-cancel
  ;;        "[" 'org-agenda-manipulate-query-add
  ;;        "g\\" 'org-agenda-filter-by-tag-refine
  ;;        "]" 'org-agenda-manipulate-query-subtract)
  ;;      ))

  ;; (defhydra hydra-org-clock (:color blue :hint nil)
  ;;   "
  ;; Clock   In/out^     ^Edit^   ^Summary     (_?_)
  ;; -----------------------------------------
  ;; _i_n         _e_dit   _g_oto entry
  ;;         _c_ontinue   _q_uit   _d_isplay
  ;;         _o_ut        ^ ^      _r_eport
  ;;       "
  ;;   ("i" org-clock-in)
  ;;   ("o" org-clock-out)
  ;;   ("c" org-clock-in-last)
  ;;   ("e" org-clock-modify-effort-estimate)
  ;;   ("q" org-clock-cancel)
  ;;   ("g" org-clock-goto)
  ;;   ("d" org-clock-display)
  ;;   ("r" org-clock-report)
  ;;   ("?" (org-info "Clocking commands")))

  ;; (defhydra hydra-org-agenda-clock (:color blue :hint nil)
  ;;   ("i" org-agenda-clock-in)
  ;;   ("o" org-agenda-clock-out)
  ;;   ("q" org-agenda-clock-cancel)
  ;;   ("g" org-agenda-clock-goto))

  ;; (bind-keys ("C-c j" . hydra-org-clock/body))
  ;; :map org-agenda-mode-map
  ;; ("C-c j" . hydra-org-agenda-clock/body))
  (pretty-hydra-define hydra-clock
    (:hint nil :foreign-keys warn :quit-key "q")
    ("Clock"
     (("q" nil "quit"))
     "Do"
     (("c" org-clock-cancel "cancel")
      ("d" org-clock-display "display")
      ("e" org-clock-modify-effort-estimate "effort")
      ("i" org-clock-in "in")
      ("j" org-clock-goto "jump")
      ("o" org-clock-out "out")
      ("r" org-clock-report "report"))))

  (defhydra hydra-org-agenda (:pre (setq which-key-inhibit t) :post (setq which-key-inhibit nil) :hint nil)
    "
Headline^^            Visit entry^^               Filter^^                    Date^^                  Toggle mode^^        View^^             Clock^^        Other^^
--------^^---------   -----------^^------------   ------^^-----------------   ----^^-------------     -----------^^------  ----^^---------    -----^^------  -----^^-----------
[_ht_] set status     [_SPC_] in other window     [_ft_] by tag               [_ds_] schedule         [_tf_] follow        [_vd_] day         [_cI_] in      [_gr_] reload
[_hk_] kill           [_TAB_] & go to location    [_fr_] refine by tag        [_dS_] un-schedule      [_tl_] log           [_vw_] week        [_cO_] out     [_._]  go to today
[_hr_] refile         [_RET_] & del other windows [_fc_] by category          [_dd_] set deadline     [_ta_] archive       [_vt_] fortnight   [_cq_] cancel  [_gd_] go to date
[_hA_] archive        [_o_]   link                [_fh_] by top headline      [_dD_] remove deadline  [_tr_] clock report  [_vm_] month       [_cj_] jump    ^^
[_h:_] set tags       ^^                          [_fx_] by regexp            [_dt_] timestamp        [_td_] diaries       [_vy_] year        ^^             ^^
[_hp_] set priority   ^^                          [_fd_] delete all filters   [_+_]  do later         ^^                   [_vn_] next span   ^^             ^^
^^                    ^^                          ^^                          [_-_]  do earlier       ^^                   [_vp_] prev span   ^^             ^^
^^                    ^^                          ^^                          ^^                      ^^                   [_vr_] reset       ^^             ^^
[_q_] quit
"
    ;; Entry
    ("h:" org-agenda-set-tags)
    ("hA" org-agenda-archive-default)
    ("hk" org-agenda-kill)
    ("hp" org-agenda-priority)
    ("hr" org-agenda-refile)
    ("ht" org-agenda-todo)

    ;; Visit entry
    ("SPC" org-agenda-show-and-scroll-up)
    ("<tab>" org-agenda-goto :exit t)
    ("TAB" org-agenda-goto :exit t)
    ("RET" org-agenda-switch-to :exit t)
    ("o"   link-hint-open-link :exit t)

    ;; Date
    ("ds" org-agenda-schedule)
    ("dS" (lambda () (interactive)
            (let ((current-prefix-arg '(4)))
              (call-interactively 'org-agenda-schedule))))
    ("dd" org-agenda-deadline)
    ("dt" org-agenda-date-prompt)
    ("dD" (lambda () (interactive)
            (let ((current-prefix-arg '(4)))
              (call-interactively 'org-agenda-deadline))))
    ("+" org-agenda-do-date-later)
    ("-" org-agenda-do-date-earlier)

    ;; View
    ("vd" org-agenda-day-view)
    ("vw" org-agenda-week-view)
    ("vt" org-agenda-fortnight-view)
    ("vm" org-agenda-month-view)
    ("vy" org-agenda-year-view)
    ("vn" org-agenda-later)
    ("vp" org-agenda-earlier)
    ("vr" org-agenda-reset-view)

    ;; Toggle mode
    ("tf" org-agenda-follow-mode)
    ("tl" org-agenda-log-mode)
    ("ta" org-agenda-archives-mode)
    ("tr" org-agenda-clockreport-mode)
    ("td" org-agenda-toggle-diary)

    ;; Filter
    ("ft" org-agenda-filter-by-tag)
    ("fr" org-agenda-filter-by-tag-refine)
    ("fc" org-agenda-filter-by-category)
    ("fh" org-agenda-filter-by-top-headline)
    ("fx" org-agenda-filter-by-regexp)
    ("fd" org-agenda-filter-remove-all)

    ;; Clock
    ("cI" org-agenda-clock-in :exit t)
    ("cj" org-agenda-clock-goto :exit t)
    ("cO" org-agenda-clock-out)
    ("cq" org-agenda-clock-cancel)

    ;; Other
    ("q" nil :exit t)
    ("gr" org-agenda-redo)
    ("." org-agenda-goto-today)
    ("gd" org-agenda-goto-date))

  ;; (defun yc/org-buffer-day-agenda ()
  ;;   (interactive)
  ;;   "Creates an agenda for the current buffer. Equivalent to the sequence: org-agenda, < (restrict to current buffer), a (agenda-list), d (org-agenda-day-view)."
  ;;   (progn
  ;;     (org-agenda-set-restriction-lock 'file)
  ;;     (org-agenda-list)
  ;;     (org-agenda-day-view))) ;; Maybe I should try writing a Emacs Lisp macro for this kind of thing!

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
           "** TODO %? %^g\n:PROPERTIES:\n:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:END:\n:LOGBOOK:\n- CREATED: %U\n:END:\n_Desired outcome:_ %^{Desired outcome:}" :clock-in t :clock-resume t :prepend t :empty-lines 1)
          ("m" "Meeting" entry (file+headline org-agenda-file-gtd "Meeting")
           "* MEETING MEETING with %? :MEETING:\n:SUBJECT:\n%T\n\n*会议内容*\n\n*重点结论*\n\n*遗留问题*\n" :clock-in t :clock-resume t :empty-lines 1)
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
          ("n" "Note" entry (file+headline org-agenda-file-notes "Notes")
           "* Note %?\n%T")
          ("N" "NOTES" entry (file+headline org-agenda-file-notes "Notes")
           "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
          ("i" "Idea" entry (file+headline org-agenda-file-thoughts "Thoughts")
           "* %? :IDEA:\n%t" :clock-in t :clock-resume t)
          ("b" "Blog idea" entry (file+headline org-default-notes-file "Blog Topics:")
           "* %?\n%T" :prepend t)
          ;; ("j" "Journal" entry (file+olp+datetree org-agenda-file-journal)
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
  
  ;; Eisenhower Matrix
  (setq org-lowest-priority ?E)
  (setq org-default-priority ?E)
  (setq org-agenda-sticky t)  ;generate differens agendas buffers in separated windows

  (add-to-list 'org-agenda-custom-commands
               '("1" "Eisenhower matrix"
                 ((tags-todo
                   "+PRIORITY=\"A\""
                   ((org-agenda-overriding-header "Urgent and important （紧急且重要 DO）"))))
                 nil))

  (add-to-list 'org-agenda-custom-commands
               '("2" "Eisenhower matrix"
                 ((tags-todo
                   "+PRIORITY=\"B\""
                   ((org-agenda-overriding-header "Important but not urgent （重要但不紧急 PLAN）"))))
                 nil))

  (add-to-list 'org-agenda-custom-commands
               '("3" "Eisenhower matrix"
                 ((tags-todo
                   "+PRIORITY=\"C\""
                   ((org-agenda-overriding-header "Urgent but not important （紧急但不重要 DELEGATE）"))))
                 nil))

  (add-to-list 'org-agenda-custom-commands
               '("4" "Eisenhower matrix"
                 ((tags-todo
                   "+PRIORITY=0-PRIORITY=\"A\"-PRIORITY=\"B\"-PRIORITY=\"C\""
                   ((org-agenda-overriding-header "Neither important nor urgent （即不重要且不紧急 ELIMINATE）"))))
                 nil))

  (defun myAgenda1 ()
    (interactive)
    (org-agenda nil "1"))

  (defun myAgenda2 ()
    (interactive)
    (org-agenda nil "2"))

  (defun myAgenda3 ()
    (interactive)
    (org-agenda nil "3"))

  (defun myAgenda4 ()
    (interactive)
    (org-agenda nil "4"))

  (defun split-4-ways ()
    (interactive)
    (delete-other-windows)
    (split-window-right)
    (split-window-below)
    (windmove-right)
    (split-window-below)
    (windmove-left))

  (defun makeAgendas ()
    (interactive)
    (myAgenda1)
    (myAgenda2)
    (myAgenda3)
    (myAgenda4))

  (defun makeMatrix ()
    (interactive)
    (makeAgendas) ;it opens: *Org Agenda(1)*, *Org Agenda(2)*, *Org Agenda(3)*, *Org Agenda(4)*
    (split-4-ways)  ;make the 4 quadrant windows
    (switch-to-buffer  "*Org Agenda(1)*")  ;put the Agenda(1) in the left, up quadrant
    (windmove-right)
    (switch-to-buffer  "*Org Agenda(2)*")  ;put the Agenda(2) in the right, up quadrant
    (windmove-down)
    (switch-to-buffer  "*Org Agenda(4)*")  ;put the Agenda(3) in the left, down quadrant
    (windmove-left)
    (switch-to-buffer  "*Org Agenda(3)*")  ;put the Agenda(4) in the right, down quadrant
    )
  ;; (global-set-key (kbd "<f5>") 'makeMatrix)
  )

;; TODO: 具体配置参考这个：
;; - https://github.com/alphapapa/org-super-agenda
;; - https://github.com/alphapapa/org-super-agenda/blob/master/examples.org
(use-package org-super-agenda
  :commands (org-agenda)
  ;;:hook ((org-agenda-mode . org-super-agenda-mode)
         ;; Easily fold groups via TAB.
  ;;       (org-super-agenda-mode . origami-mode))
  :bind (:map org-super-agenda-header-map ("<tab>" . origami-toggle-node))
  :init
  (org-super-agenda-mode t)
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
          ))
  ;; :config
  ;; (org-super-agenda-mode t)
  )

;; - https://github.com/alphapapa/org-ql
;; org-ql 常用命令的使用说明 https://emacs-china.org/t/org-ql/21486/2
(use-package org-ql
  :commands (org-ql-view org-ql-search))

;; Pomodoro
(use-package org-pomodoro
  :after org org-agenda
  :general (org-agenda-mode-map
            "P" 'org-pomodoro))

;; Add graphical view of agenda
;; (use-package org-timeline
;;   :hook (org-agenda-finalize . org-timeline-insert-timeline))

(provide 'init-org-agenda)