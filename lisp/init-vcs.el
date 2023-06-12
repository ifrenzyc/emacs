;; init-vcs.el --- Initialize vcs settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code

;; 设定不产生备份文件
(setq make-backup-files nil)
(setq-default make-backup-files nil)    ; 不生成临时文件

;; 取消自动保存模式
(setq auto-save-mode nil)

(setq backup-by-copying nil)

(use-package git-modes
  ;; :straight (:host github :repo "magit/git-modes")
  :after (magit)
  :config
  (use-package gitignore-mode
    :after (magit))
  (use-package gitattributes-mode
    :after (magit)))

;; - https://emacsair.me/2017/09/01/magit-walk-through/
(use-package magit
  :commands (magit-status)
  :general
  ("C-x g" 'magit-status
   "C-x G" 'magit-log-buffer-file)
  :hook
  (magit-mode . (lambda ()
                  (selected-minor-mode -1)))
  (magit-log-edit-mode . (lambda ()
                           (set-fill-column 89)
                           (auto-fill-mode 1)))
  
  ((magit-git-executable "/usr/bin/git")
   (magit-log-arguments '("-n256" "--graph" "--decorate" "--color"))
   ;; magit-completing-read-function 'magit-ido-completing-read                        ; use ido to look for branches
   (magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)       ; don't put "origin-" in front of new branch names by default
   ;; Show diffs per word, looks nicer!
   (magit-diff-refine-hunk t)        ; highlight word/letter changes in hunk diffs
   (magit-save-some-buffers nil))    ; don't attempt to save unsaved buffers
  :config
  ;; (when (featurep 'evil)
  ;;   (setq evil-emacs-state-modes (delq 'magit-status-mode evil-emacs-state-modes)))

  (when (fboundp 'transient-append-suffix)
    ;; Add switch: --tags
    (transient-append-suffix 'magit-fetch
      "-p" '("-t" "Fetch all tags" ("-t" "--tags"))))

  (diminish 'magit-auto-revert-mode ""))

;; highlight git changes.
;; (use-package git-gutter
;;   :config (global-git-gutter-mode)
;;   (defhydra hydra-git-gutter (:body-pre (git-gutter-mode 1)
;;                                         :hint nil)
;;     ("n" git-gutter:next-hunk "next hunk")
;;     ("p" git-gutter:previous-hunk "previous hunk")
;;     ("N" git-gutter+-next-hunk "next hunk+")
;;     ("P" git-gutter+-previous-hunk "previous hunk+")
;;     ("h" (progn (goto-char (point-min)) (git-gutter:next-hunk 1)) "first hunk")
;;     ("l" (progn (goto-char (point-min)) (git-gutter:previous-hunk 1)) "last hunk")
;;     ("<SPC>" git-gutter:popup-hunk "popup hunk")
;;     ("s" git-gutter:stage-hunk "stage hunk")
;;     ("r" git-gutter:revert-hunk "revert hunk")
;;     ("q" nil "quit")))

;; (use-package git-gutter+
;;   :after (git-gutter))

;; A git blame plugin for emacs inspired by VS Code’s GitLens plugin and Vim plugin
;; https://codeberg.org/akib/emacs-why-this
;; (use-package blamer
;;   :straight (:host github :repo "artawower/blamer.el")
;;   :init
;;   (setq blamer-idle-time 0.3)
;;   (setq blamer-min-offset 70)
;;   :custom-face
;;   (blamer-face ((t :foreground "#7a88cf"
;;                    :background nil
;;                    :height 140
;;                    :italic t)))
;;   :config
;;   (global-blamer-mode 1))

;; Pop up last commit information of current line
;; https://github.com/syohex/emacs-git-messenger
;; copy from: https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-vcs.el#L79
;; - vc-msg
(use-package git-messenger
  :bind (:map vc-prefix-map
              ("p" . git-messenger:popup-message)
              :map git-messenger-map
              ("m" . git-messenger:copy-message))
  :init (setq git-messenger:show-detail t
              git-messenger:use-magit-popup t)
  :config
  ;; TODO 这里应该可以用 major-mode-hydra 来绑定
  (with-no-warnings
    (with-eval-after-load 'hydra
      (defhydra git-messenger-hydra (:color blue)
        ("s" git-messenger:popup-show "show")
        ("c" git-messenger:copy-commit-id "copy hash")
        ("m" git-messenger:copy-message "copy message")
        ("," (catch 'git-messenger-loop (git-messenger:show-parent)) "go parent")
        ("q" git-messenger:popup-close "quit")))

    (defun my-git-messenger:format-detail (vcs commit-id author message)
      (if (eq vcs 'git)
          (let ((date (git-messenger:commit-date commit-id))
                (colon (propertize ":" 'face 'font-lock-comment-face)))
            (concat
             (format "%s%s %s \n%s%s %s\n%s  %s %s \n"
                     (propertize "Commit" 'face 'font-lock-keyword-face) colon
                     (propertize (substring commit-id 0 8) 'face 'font-lock-comment-face)
                     (propertize "Author" 'face 'font-lock-keyword-face) colon
                     (propertize author 'face 'font-lock-string-face)
                     (propertize "Date" 'face 'font-lock-keyword-face) colon
                     (propertize date 'face 'font-lock-string-face))
             (propertize (make-string 38 ?─) 'face 'font-lock-comment-face)
             message
             (propertize "\nPress q to quit" 'face '(:inherit (font-lock-comment-face italic)))))
        (git-messenger:format-detail vcs commit-id author message)))

    (defun my-git-messenger:popup-message ()
      "Popup message with `posframe', `pos-tip', `lv' or `message', and dispatch actions with `hydra'."
      (interactive)
      (let* ((hydra-hint-display-type 'message)
             (vcs (git-messenger:find-vcs))
             (file (buffer-file-name (buffer-base-buffer)))
             (line (line-number-at-pos))
             (commit-info (git-messenger:commit-info-at-line vcs file line))
             (commit-id (car commit-info))
             (author (cdr commit-info))
             (msg (git-messenger:commit-message vcs commit-id))
             (popuped-message (if (git-messenger:show-detail-p commit-id)
                                  (my-git-messenger:format-detail vcs commit-id author msg)
                                (cl-case vcs
                                  (git msg)
                                  (svn (if (string= commit-id "-")
                                           msg
                                         (git-messenger:svn-message msg)))
                                  (hg msg)))))
        (setq git-messenger:vcs vcs
              git-messenger:last-message msg
              git-messenger:last-commit-id commit-id)
        (run-hook-with-args 'git-messenger:before-popup-hook popuped-message)
        (git-messenger-hydra/body)
        (cond ((and (fboundp 'posframe-workable-p) (posframe-workable-p))
               (let ((buffer-name "*git-messenger*"))
                 (posframe-show buffer-name
                                :string (concat (propertize "\n" 'face '(:height 0.3))
                                                popuped-message
                                                "\n"
                                                (propertize "\n" 'face '(:height 0.3)))
                                :left-fringe 8
                                :right-fringe 8
                                :internal-border-width 1
                                :internal-border-color (face-foreground 'font-lock-comment-face nil t)
                                :background-color (face-background 'tooltip nil t))
                 (unwind-protect
                     (push (read-event) unread-command-events)
                   (posframe-delete buffer-name))))
              ((and (fboundp 'pos-tip-show) (display-graphic-p))
               (pos-tip-show popuped-message))
              ((fboundp 'lv-message)
               (lv-message popuped-message)
               (unwind-protect
                   (push (read-event) unread-command-events)
                 (lv-delete-window)))
              (t (message "%s" popuped-message)))
        (run-hook-with-args 'git-messenger:after-popup-hook popuped-message)))
    (advice-add #'git-messenger:popup-close :override #'ignore)
    (advice-add #'git-messenger:popup-message :override #'my-git-messenger:popup-message)))

;; Display transient in child frame
;; (use-package transient-posframe
;;   :diminish
;;   :custom-face
;;   (transient-posframe-border ((t (:background ,(face-foreground 'font-lock-comment-face)))))
;;   :hook (after-init . transient-posframe-mode)
;;   :init
;;   (setq transient-posframe-border-width 3
;;         transient-posframe-min-height 21
;;         transient-posframe-min-width nil
;;         transient-posframe-parameters
;;         `((background-color . ,(face-background 'tooltip))))
;;   :config
;;   (add-hook 'after-load-theme-hook
;;             (lambda ()
;;               (posframe-delete-all)
;;               (custom-set-faces
;;                `(transient-posframe-border
;;                  ((t (:background ,(face-foreground 'font-lock-comment-face))))))
;;               (setf (alist-get 'background-color transient-posframe-parameters)
;;                     (face-background 'tooltip))))

;;   (with-no-warnings
;;     (defun my-transient-posframe--show-buffer (buffer _alist)
;;       "Show BUFFER in posframe and we do not use _ALIST at this period."
;;       (when (posframe-workable-p)
;;         (let ((posframe (posframe-show
;;                          buffer
;; 			             :font transient-posframe-font
;; 			             :position (point)
;; 			             :poshandler transient-posframe-poshandler
;; 			             :background-color (face-attribute 'transient-posframe :background nil t)
;; 			             :foreground-color (face-attribute 'transient-posframe :foreground nil t)
;; 			             :min-width (or transient-posframe-min-width (round (* (frame-width) 0.62)))
;; 			             :min-height transient-posframe-min-height
;;                          :lines-truncate t
;; 			             :internal-border-width transient-posframe-border-width
;; 			             :internal-border-color (face-attribute 'transient-posframe-border :background nil t)
;; 			             :override-parameters transient-posframe-parameters)))
;;           (frame-selected-window posframe))))
;;     (advice-add #'transient-posframe--show-buffer :override #'my-transient-posframe--show-buffer)))

(use-package magit-todos
  :after magit
  :commands (magit-todos-mode)
  :config
  (setq magit-todos-recursive t)
  (setq magit-todos-depth 100)
  (setq magit-todos-keyword-suffix "\\(?:([^)]+)\\)?:?") ; make colon optional
  (setq magit-todos-group-by
        '(magit-todos-item-first-path-component magit-todos-item-keyword magit-todos-item-filename))
  (setq magit-todos-exclude-globs '("nixpkgs/*"))
  (custom-set-variables
   '(magit-todos-keywords (list "TODO" "FIXME"))))

(use-package magit-stats)

;; - https://github.com/emacs-evil/evil-magit

(use-package git-timemachine
  :custom-face
  (git-timemachine-minibuffer-author-face ((t (:inherit success))))
  (git-timemachine-minibuffer-detail-face ((t (:inherit warning))))
  :bind (:map vc-prefix-map
              ("t" . git-timemachine))
  :hook ((before-revert . (lambda ()
                            (when (bound-and-true-p git-timemachine-mode)
                              (user-error "Cannot revert the timemachine buffer")))))
  (git-timemachine-mode . hydra-git-timemachine/body)
  :config
  ;; (eval-after-load 'git-timemachine
  ;;   '(progn
  ;;      (evil-make-overriding-map git-timemachine-mode-map 'normal)
  ;;      ;; force update evil keymaps after git-timemachine-mode loaded
  ;;      (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)))

  (defhydra hydra-git-timemachine ()
    "Git timemachine"
    ("p" git-timemachine-show-previous-revision "previous revision")
    ("n" git-timemachine-show-next-revision "next revision")
    ("q" nil "quit"))
  )

;; - https://github.com/thisch/gerrit.el
;; (use-package gerrit
;;   :custom
;;   (gerrit-host "review.opendev.org")  ;; is needed for REST API calls
;;   (gerrit-use-gitreview-interface nil)
;;   :config
;;   (progn
;;     (add-hook 'magit-status-sections-hook #'gerrit-magit-insert-status t)
;;     ;; (global-set-key (kbd "C-x i") 'gerrit-upload-transient)
;;     ;; (global-set-key (kbd "C-x o") 'gerrit-download)
;;     ))

;; forge: Access Git forges for Magit
;; https://github.com/magit/forge
(use-package forge
  :disabled t
  :after magit
  :bind ((:map forge-issue-section-map
               ("C-c C-v" . forge-browse-topic))
         (:map forge-pullreq-section-map
               ("C-c C-v" . forge-browse-topic))))

;; highcontrast git diff style
;; macOS:
;;    brew install git-delta
;; config ~/.gitconfig file
;; - https://github.com/dandavison/delta/
(use-package magit-delta
  :disabled t
  ;; :straight (:host github :repo "dandavison/magit-delta")
  :hook (magit-mode . magit-delta-mode))

(provide 'init-vcs)
