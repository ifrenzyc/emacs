;; init-mail.el --- Initialize mail settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; - mu4e
;; - notmuch
;; 参考 /Users/yangc/src/emacs.d/dakra-dmacs/init.org，使用 mu4e 查收邮件
;; 
;; - Mike Zamansky 视频 https://www.youtube.com/watch?v=newRHXKm4H4
;; - https://www.reddit.com/r/emacs/comments/bfsck6/mu4e_for_dummies/
;; - https://coldnew.github.io/6a7aa5c1/
;; - http://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and-/
;; - https://rakhim.org/fastmail-setup-with-emacs-mu4e-and-mbsync-on-macos/
;; - https://emacs.lujianmei.com/init-daily-using/init-mu4e
;; 
;; GitHub 上，这两个工程 https://github.com/cemkeylan/mu-wizard、https://github.com/JackMoffat/Mu4Easy 提供简化 mail 的脚本，思路也是用 mbsync offlineimap mu 实现。
;; 

;;; Code
(use-package smtpmail
  :init
  (setq smtpmail-default-smtp-server "smtp.chinatelecom.cn")
  :config
  (setq smtpmail-smtp-server "smtp.chinatelecom.cn")
  (setq smtpmail-stream-type 'ssl)
  (setq smtpmail-smtp-service 465)
  (setq smtpmail-queue-mail nil))

;; (use-package smtpmail-async
;;   :after smtpmail
;;   :config
;;   (setq send-mail-function 'async-smtpmail-send-it)
;;   (setq message-send-mail-function 'async-smtpmail-send-it))

(use-package mu4e
  :load-path "localelpa/mu4e/mu4e"
  :commands (mu4e)
  ;; Open mu4e with the 'Mail' key (if your keyboard has one)
  :bind (
         :map mu4e-main-mode-map
         ("U" . mu4e-update-mail-and-index-background)
         :map mu4e-headers-mode-map
         ("TAB" . mu4e-headers-next-unread)
         ("d" . my-move-to-trash)
         ("D" . my-move-to-trash)
         ("M" . mu4e-headers-mark-all-unread-read) ; Mark all as read
         :map mu4e-view-mode-map
         ("A" . mu4e-view-attachment-action)
         ("M-o" . ace-link-mu4e)
         ("o" . ace-link-mu4e)
         ("n" . mu4e-scroll-up)
         ("p" . mu4e-scroll-down)
         ("N" . mu4e-view-headers-next)
         ("P" . mu4e-view-headers-prev)
         ("d" . my-move-to-trash)
         ("D" . my-move-to-trash))
  :init
  ;; Use gnus view-mode by default
  (setq mu4e-view-use-gnus t)
  ;; and prefer text over html/ritchtext
  (setq mm-discouraged-alternatives '("text/html" "text/richtext"))

  ;; Use completing-read (which is ivy) instead of ido
  (setq mu4e-completing-read-function 'completing-read)

  ;; set mu4e as default mail client
  (setq mail-user-agent 'mu4e-user-agent)

  ;; Always use local smtp server (msmtp in my case) to send mails
  (setq send-mail-function 'sendmail-send-it
        sendmail-program "~/bin/msmtp-enqueue.sh"
        mail-specify-envelope-from t
        message-sendmail-f-is-evil nil
        mail-envelope-from 'header
        message-sendmail-envelope-from 'header)
  :config
  (defun mu4e-update-mail-and-index-background ()
    "Call `mu4e-update-mail-and-index' to run in background."
    (interactive)
    (mu4e-update-mail-and-index t))

  ;; gmail delete == move mail to trash folder
  (fset 'my-move-to-trash "mt")

  ;; Fix mu4e highlighting in moe-dark theme
  (set-face-attribute 'mu4e-header-highlight-face nil :background "#626262" :foreground "#eeeeee")

  ;;; Save attachment (this can also be a function)
  (setq-default mu4e-attachment-dir "~/Downloads")

  ;; When saving multiple attachments (C-u prefix) save all in same directory
  ;; without asking for the location of every attachment
  (setq-default mu4e-save-multiple-attachments-without-asking t)

  ;; Always display plain text messages.
  (setq mu4e-view-html-plaintext-ratio-heuristic most-positive-fixnum)

  (setq mu4e-msg2pdf "/usr/bin/msg2pdf")  ; to display html messages as pdf

  ;; Show additional user-agent header
  (setq-default mu4e-view-fields
                '(:from :to :cc :subject :flags :date :maildir :user-agent :mailing-list
                        :tags :attachments :signature :decryption))

  ;; Attach file with helm-locate
  ;;(helm-add-action-to-source "Attach to Email" #'mml-attach-file helm-source-locate)

  ;; default
  (setq-default mu4e-maildir "~/Maildir")
  (setq-default mu4e-drafts-folder "/private/Drafts")
  (setq-default mu4e-sent-folder   "/private/Sent")
  (setq-default mu4e-trash-folder  "/private/Trash")

  ;; Setup some handy shortcuts
  ;; you can quickly switch to your Inbox -- press ``ji''
  ;; then, when you want archive some messages, move them to
  ;; the 'All Mail' folder by pressing ``ma''.

  (setq mu4e-maildir-shortcuts
        '(("/private/Inbox"      . ?i)
          ("/private/Sent"       . ?s)
          ("/private/Trash"      . ?t)
          ("/private/Drafts"     . ?d)
          ("/private/Archive"   . ?a)))

  ;; Dynamically refile
  ;; See: https://www.djcbsoftware.nl/code/mu/mu4e/Smart-refiling.html#Smart-refiling
  (defun dakra-mu4e-private-refile (msg)
    (cond
     ;; refile all messages from Uber to the 'uber' folder
     ((mu4e-message-contact-field-matches msg :from "@uber\\.com")
      "/private/uber")
     ;; important to have a catch-all at the end!
     (t  "/private/Archive")))

  (setq-default mu4e-refile-folder 'dakra-mu4e-private-refile)

  ;; Don't show duplicate mails when searching
  (setq-default mu4e-headers-skip-duplicates t)

  ;; Show email address as well and not only the name
  (setq-default mu4e-view-show-addresses t)

  ;; Don't show related messages by default.
  ;; Activate with 'W' on demand
  (setq-default mu4e-headers-include-related nil)

  ;; Don't ask to quit
  (setq-default mu4e-confirm-quit nil)

  ;; Don't spam the minibuffer with 'Indexing...' messages
  (setq-default mu4e-hide-index-messages t)

  ;; Always update in background otherwise mu4e manipulates the window layout
  ;; when the update is finished but this breaks when we switch exwm workspaces
  ;; and the current focused window just gets hidden.
  (setq-default mu4e-index-update-in-background t)

  ;; Add some mailing lists
  (dolist (mailing-list '(("intern.lists.entropia.de" . "Entropia")
                          ("intern.lists.ccc.de" . "CCC")
                          ("pylons-discuss.googlegroups.com" . "PyrUsr")
                          ("pylons-devel.googlegroups.com" . "PyrDev")
                          ("sqlalchemy.googlegroups.com" . "SQLA")))
    (add-to-list 'mu4e~mailing-lists mailing-list))

  (setq mu4e-bookmarks `((,(concat "maildir:/private/Inbox OR "
                                   "maildir:/paessler/Inbox OR "
                                   "maildir:/sap/Inbox OR "
                                   "maildir:/gmail/inbox OR "
                                   "maildir:/atomx/inbox OR "
                                   "maildir:/hogaso/inbox OR "
                                   "maildir:/e5/Inbox")
                          "All inboxes" ?i)
                         ("flag:flagged" "Flagged messages" ?f)
                         (,(concat "flag:unread AND "
                                   "NOT flag:trashed AND "
                                   "NOT flag:seen AND "
                                   "NOT list:emacs-devel.gnu.org AND "
                                   "NOT list:emacs-orgmode.gnu.org AND "
                                   "NOT maildir:/private/Junk AND "
                                   "NOT maildir:/private/Trash AND "
                                   "NOT maildir:/atomx/spam AND "
                                   "NOT maildir:/atomx/trash AND "
                                   "NOT maildir:/paessler/Deleted\\ Items AND "
                                   "NOT maildir:/sap/Deleted\\ Items AND "
                                   "NOT maildir:/gmail/spam AND "
                                   "NOT maildir:/gmail/trash")
                          "Unread messages" ?a)
                         (,(concat "flag:unread AND "
                                   "NOT flag:trashed AND "
                                   "NOT flag:seen AND "
                                   "NOT maildir:/private/Junk AND "
                                   "NOT maildir:/private/Trash AND "
                                   "NOT maildir:/atomx/spam AND "
                                   "NOT maildir:/atomx/trash AND "
                                   "NOT maildir:/paessler/Deleted\\ Items AND "
                                   "NOT maildir:/sap/Deleted\\ Items AND "
                                   "NOT maildir:/gmail/spam AND "
                                   "NOT maildir:/gmail/trash")
                          "All Unread messages" ?A)
                         ("list:emacs-devel.gnu.org" "Emacs dev" ?d)
                         ("list:emacs-orgmode.gnu.org" "Emacs orgmode" ?o)
                         ("list:magit.googlegroups.com OR list:mu-discuss.googlegroups.com" "Elisp" ?e)
                         ("list:pylons-discuss.googlegroups.com OR list:pylons-devel.googlegroups.com OR list:sqlalchemy.googlegroups.com" "Python" ?p)
                         ("list:intern.lists.ccc.de" "CCC Intern" ?c)
                         ("list:intern.lists.entropia.de" "Entropia Intern" ?k)
                         ("list:uwsgi.lists.unbit.it" "uwsgi" ?u)))

  ;; (add-hook 'mu4e-mark-execute-pre-hook
  ;;           (lambda (mark msg)
  ;;             (cond ((member mark '(refile trash)) (mu4e-action-retag-message msg "-\\Inbox"))
  ;;                   ((equal mark 'flag) (mu4e-action-retag-message msg "\\Starred"))
  ;;                   ((equal mark 'unflag) (mu4e-action-retag-message msg "-\\Starred")))))

  ;; allow for updating mail using 'U' in the main view:
  ;; (only update inboxes)
  (setq mu4e-get-mail-command "mbsync private:Inbox paessler:Inbox e5:Inbox gmail-inbox chinatelecom-inbox hogaso-inbox")
  ;; for update all:
  ;;(setq mu4e-get-mail-command "mbsync -a")

  ;; update database every ten minutes
  ;; (setq  mu4e-update-interval (* 60 10))
  (setq  mu4e-update-interval nil)

  ;; We do a full index (that verify integrity) with a systemd job
  ;; Go fast inside emacs
  (setq mu4e-index-cleanup nil      ;; don't do a full cleanup check
        mu4e-index-lazy-check t)    ;; don't consider up-to-date dirs

  ;;; Use 'fancy' non-ascii characters in various places in mu4e
  (setq mu4e-use-fancy-chars t)
  ;; And change default threading characters to some "nicer" looking chars
  (setq mu4e-headers-thread-child-prefix '("├>" . "├▶ "))
  (setq mu4e-headers-thread-last-child-prefix '("└>" . "└▶ "))
  (setq mu4e-headers-thread-connection-prefix '("│" . "│ "))
  (setq mu4e-headers-thread-orphan-prefix '("┬>" . "┬▶ "))
  (setq mu4e-headers-thread-single-orphan-prefix '("─>" . "─▶ "))

  ;; Also change to some nicer characters for marks
  (setq mu4e-headers-new-mark    '("N" . "☉"))
  (setq mu4e-headers-passed-mark  '("P" . "›"))
  (setq mu4e-headers-replied-mark '("R" . "‹"))
  (setq mu4e-headers-seen-mark   '("S" . ""))
  (setq mu4e-headers-attach-mark '("a" . "📎"))
  (setq mu4e-headers-unread-mark '("u" . "✉"))

  ;; I want my format=flowed thank you very much
  ;; mu4e sets up visual-line-mode and also fill (M-q) to do the right thing
  ;; each paragraph is a single long line; at sending, emacs will add the
  ;; special line continuation characters.
  (setq mu4e-compose-format-flowed nil)

  ;; Don't open new frame for composing mails
  (setq mu4e-compose-in-new-frame nil)

  ;; Don't reply to self
  (setq mu4e-user-mail-address-list
        '("daniel@kraus.my" "daniel.kraus@gmail.com" "dakra@tr0ll.net" "daniel@tr0ll.net" "d@niel-kraus.de"
          "arlo@kraus.my"
          "dakra-cepheus@tr0ll.net"
          "daniel@skor.buzz"
          "daniel@atomx.com"
          "daniel@hogaso.com"
          "daniel@restalchemy.org" "daniel@restalchemy.com"
          "d.kraus@sap.com"
          "daniel.kraus@paessler.com"
          "daniel.kraus@ebenefuenf.de"))
  (setq mu4e-compose-dont-reply-to-self t)

  ;; Extract name from email for yasnippet template
  ;; http://pragmaticemacs.com/emacs/email-templates-in-mu4e-with-yasnippet/
  (defun bjm/mu4e-get-names-for-yasnippet ()
    "Return comma separated string of names for an email"
    (interactive)
    (let ((email-name "") str email-string email-list email-name2 tmpname)
      (save-excursion
        (goto-char (point-min))
        ;; first line in email could be some hidden line containing NO to field
        (setq str (buffer-substring-no-properties (point-min) (point-max))))
      ;; take name from TO field - match series of names
      (when (string-match "^To: \"?\\(.+\\)" str)
        (setq email-string (match-string 1 str)))
      ;;split to list by comma
      (setq email-list (split-string email-string " *, *"))
      ;;loop over emails
      (dolist (tmpstr email-list)
        ;;get first word of email string
        (setq tmpname (car (split-string tmpstr " ")))
        ;;remove whitespace or ""
        (setq tmpname (replace-regexp-in-string "[ \"]" "" tmpname))
        ;;join to string
        (setq email-name
              (concat email-name ", " tmpname)))
      ;;remove initial comma
      (setq email-name (replace-regexp-in-string "^, " "" email-name))

      ;;see if we want to use the name in the FROM field
      ;;get name in FROM field if available, but only if there is only
      ;;one name in TO field
      (if (< (length email-list) 2)
          (when (string-match "^\\([^ ,\n]+\\).+writes:$" str)
            (progn (setq email-name2 (match-string 1 str))
                   ;;prefer name in FROM field if TO field has "@"
                   (when (string-match "@" email-name)
                     (setq email-name email-name2))
                   )))
      email-name))

  ;; Always store contacts as first last <email>
  ;; https://martinralbrecht.wordpress.com/2016/05/30/handling-email-with-emacs/
  (defun malb/canonicalise-contact-name (name)
    (let ((case-fold-search nil))
      (setq name (or name ""))
      (if (string-match-p "^[^ ]+@[^ ]+\.[^ ]" name)
          ""
        (progn
          ;; drop email address
          (setq name (replace-regexp-in-string "^\\(.*\\) [^ ]+@[^ ]+\.[^ ]" "\\1" name))
          ;; strip quotes
          (setq name (replace-regexp-in-string "^\"\\(.*\\)\"" "\\1" name))
          ;; deal with YELL’d last names
          (setq name (replace-regexp-in-string "^\\(\\<[[:upper:]]+\\>\\) \\(.*\\)" "\\2 \\1" name))
          ;; Foo, Bar becomes Bar Foo
          (setq name (replace-regexp-in-string "^\\(.*\\), \\([^ ]+\\).*" "\\2 \\1" name))))))

  (defun malb/mu4e-contact-rewrite-function (contact)
    (let* ((name (or (plist-get contact :name) ""))
           ;; (mail (plist-get contact :mail))
           (case-fold-search nil))
      (plist-put contact :name (malb/canonicalise-contact-name name))
      contact))

  (setq mu4e-contact-rewrite-function #'malb/mu4e-contact-rewrite-function)


  (defun dakra-mu4e-action-attachment-import-gcalcli (msg attachnum)
    "Import ical attachments with gcalcli"
    (mu4e-view-open-attachment-with msg attachnum "~/bin/icalimport.sh"))

  (add-to-list 'mu4e-view-attachment-actions '("iImport ical" . dakra-mu4e-action-attachment-import-gcalcli) t)

  (defun mu4e-action-view-in-firefox (msg)
    "View the body of the message in a new Firefox window."
    (let ((browse-url-browser-function 'browse-url-firefox)
          (browse-url-new-window-flag t))
      (browse-url (concat "file://" (mu4e~write-body-to-html msg)))))

  ;; View mail in browser with "a V"
  (add-to-list 'mu4e-view-actions
               '("ViewInBrowser" . mu4e-action-view-in-browser) t)
  (add-to-list 'mu4e-view-actions
               '("fViewInFirefox" . mu4e-action-view-in-firefox) t)
  (add-to-list 'mu4e-view-actions
               '("xViewXWidget" . mu4e-action-view-with-xwidget) t)
  ;; enable inline images
  (setq mu4e-view-show-images t)
  ;; use imagemagick, if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  ;;rename files when moving
  ;;NEEDED FOR MBSYNC
  (setq mu4e-change-filenames-when-moving t)

  (defun mu4e-message-maildir-matches (msg rx)
    "Match message MSG with regex RX based on maildir."
    (when rx
      (if (listp rx)
          ;; if rx is a list, try each one for a match
          (or (mu4e-message-maildir-matches msg (car rx))
              (mu4e-message-maildir-matches msg (cdr rx)))
        ;; not a list, check rx
        (string-match rx (mu4e-message-field msg :maildir)))))

  (defmacro mu4e-context-match-fun (maildir)
    "Return lambda for context switching which checks if a message is in MAILDIR."
    `(lambda (msg)
       (when msg
         (mu4e-message-maildir-matches msg ,maildir))))

  (setq mu4e-contexts
        `(
          ;; ,(make-mu4e-context
          ;;   :name "private"
          ;;   :enter-func (lambda () (mu4e-message "Switch to the Private context"))
          ;;   :match-func (mu4e-context-match-fun "^/private")
          ;;   :vars '((gnus-icalendar-org-capture-file "~/org/personal.org")
          ;;           ( user-mail-address  . "daniel@kraus.my" )
          ;;           ( mu4e-maildir-shortcuts . (("/private/Inbox"      . ?i)
          ;;                                       ("/private/Sent"       . ?s)
          ;;                                       ("/private/Trash"      . ?t)
          ;;                                       ("/private/Drafts"     . ?d)
          ;;                                       ("/private/Archive"   . ?a)))
          ;;           ( mu4e-drafts-folder . "/private/Drafts" )
          ;;           ( mu4e-sent-folder   . "/private/Sent" )
          ;;           ( mu4e-trash-folder  . "/private/Trash" )
          ;;           ( mu4e-refile-folder . dakra-mu4e-private-refile)))
          ,(make-mu4e-context
            :name "gmail"
            :enter-func (lambda () (mu4e-message "Switch to the gmail context"))
            :match-func (mu4e-context-match-fun "^/gmail")
            :vars '(;; (gnus-icalendar-org-capture-file "~/org/personal.org")
                    ( user-mail-address  . "ifrenzyc@gmail.com"  )
                    ( mu4e-maildir-shortcuts . (("/gmail/inbox"      . ?i)
                                                ("/gmail/sent_mail"  . ?s)
                                                ("/gmail/trash"      . ?t)
                                                ("/gmail/drafts"     . ?d)
                                                ("/gmail/all_mail"   . ?a)))
                    ( mu4e-drafts-folder . "/gmail/drafts" )
                    ( mu4e-sent-folder   . "/gmail/sent_mail" )
                    ( mu4e-trash-folder  . "/gmail/trash" )
                    ( mu4e-refile-folder . "/gmail/all_mail" )
                    ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
                    ( mu4e-sent-messages-behavior  . delete)))
          ,(make-mu4e-context
            :name "chinatelecom"
            :enter-func (lambda () (mu4e-message "Switch to the chinatelecom context"))
            :match-func (mu4e-context-match-fun "^/chinatelecom")
            :vars '(( user-mail-address  . "yangchuang.gd@chinatelecom.cn" )
                    ( mu4e-maildir-shortcuts . (("/chinatelecom/inbox"      . ?i)
                                                ("/chinatelecom/sent_mail"  . ?s)
                                                ("/chinatelecom/trash"      . ?t)
                                                ("/chinatelecom/drafts"     . ?d)
                                                ("/chinatelecom/all_mail"   . ?a)))
                    ( mu4e-drafts-folder . "/chinatelecom/drafts" )
                    ( mu4e-sent-folder   . "/chinatelecom/sent_mail" )
                    ( mu4e-trash-folder  . "/chinatelecom/trash" )
                    ( mu4e-refile-folder . "/chinatelecom/all_mail" )
                    ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
                    ( mu4e-sent-messages-behavior  . delete)))))

  ;; start with the first (default) context;
  ;; default is to ask-if-none (ask when there's no context yet, and none match)
  (setq mu4e-context-policy 'pick-first)

  ;; compose with the current context is no context matches;
  ;; default is to ask
  '(setq mu4e-compose-context-policy nil)

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

  ;; something about ourselves
  ;; (setq
  ;;  user-mail-address "daniel@kraus.my"
  ;;  user-full-name  "Daniel Kraus"
  ;;  mu4e-compose-signature
  ;;  (concat
  ;;   "regards,\n"
  ;;   "  Daniel\n"))

  ;; If there's 'attach' 'file' 'pdf' in the message warn when sending w/o attachment
  (defun mbork/message-attachment-present-p ()
    "Return t if an attachment is found in the current message."
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (when (search-forward "<#part" nil t) t))))

  (defcustom mbork/message-attachment-intent-re
    (regexp-opt '("attach"
                  "anhang"
                  "angehängt"
                  "angehaengt"
                  "datei"
		          "file"
                  "pdf"))
    "A regex which - if found in the message, and if there is no
attachment - should launch the no-attachment warning."
    :type '(sexp)
    :group 'mu4e)

  (defcustom mbork/message-attachment-reminder
    "Are you sure you want to send this message without any attachment? "
    "The default question asked when trying to send a message
containing `mbork/message-attachment-intent-re' without an
actual attachment."
    :type '(string)
    :group 'mu4e)

  (defun mbork/message-warn-if-no-attachments ()
    "Ask the user if s?he wants to send the message even though
there are no attachments."
    (when (and (save-excursion
	             (save-restriction
		           (widen)
		           (goto-char (point-min))
		           (re-search-forward mbork/message-attachment-intent-re nil t)))
	           (not (mbork/message-attachment-present-p)))
      (unless (y-or-n-p mbork/message-attachment-reminder)
        (keyboard-quit))))

  (add-hook 'message-send-hook #'mbork/message-warn-if-no-attachments))

;; Show overview of unread/all mails for each maildir/bookmarks in mu4e main window
(use-package mu4e-maildirs-extension
  :disabled t
  :commands mu4e-maildirs-extension-force-update
  :config
  (setq-default mu4e-maildirs-extension-use-bookmarks t)
  (setq-default mu4e-maildirs-extension-use-maildirs nil)
  (mu4e-maildirs-extension))

;; (define-key mu4e-headers-mode-map (kbd "d") 'my-move-to-trash)
;; (define-key mu4e-view-mode-map (kbd "d") 'my-move-to-trash)
;; ;; Overwrite normal 'D' keybinding
;; (define-key mu4e-headers-mode-map (kbd "D") 'my-move-to-trash)
;; (define-key mu4e-view-mode-map (kbd "D") 'my-move-to-trash)

;; ;; Mark all as read with 'M'
;; (define-key mu4e-headers-mode-map (kbd "M") 'mu4e-headers-mark-all-unread-read)

;; provide counsel integrate.
;; - https://github.com/seanfarley/counsel-mu
(use-package counsel-mu
  :load-path "localelpa/counsel-mu"
  :commands (counsel-mu4e))

;; - https://github.com/seanfarley/mu4e-patch
(use-package mu4e-patch
  :load-path "localelpa/mu4e-patch"
  :config
  ;; colorize patch-based emails
  (add-hook 'mu4e-view-mode-hook #'mu4e-patch-highlight))

(provide 'init-mail)
