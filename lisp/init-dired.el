;; init-dired.el --- Initialize dired configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:
;; - https://jonathanabennett.github.io/blog/2019/06/05/file-management-in-emacs-with-dired-mode/
;; - https://stackoverflow.com/questions/23798021/disabling-evil-mode-for-nav-in-emacs-or-any-read-only-buffers
(use-package dired
  :ensure nil
  :hook
  ((dired-mode . hl-line-mode)
   (dired-mode . dired-hide-details-mode))
  :custom
  ;; show human readable file sizes in dired
  ;; http://pragmaticemacs.com/emacs/dired-human-readable-sizes-and-sort-by-size/
  ;; (setq dired-listing-switches "-aBhl --group-directories-first -v")  ; 这个目前会导致带有中文名字的文件名显示成转义字符
  ;; (setq dired-listing-switches "-AFhlv --dired-listing-switches")
  (dired-listing-switches "-alh")

  ;; This allows dired to copy/paste/move files over to the other directory in a separate window pane quickly.
  (dired-dwim-target t)     ;; https://emacs.stackexchange.com/a/5604
  (dired-recursive-copies 'always) ;; Recursive Copying and Deleting
  (dired-recursive-deletes 'always)
  :config
  (defun yc/dired-up-directory ()
    "Take dired up one directory, but behave like dired-find-alternative-file (leave no orphan buffer)"
    (interactive)
    (let ((old (current-buffer)))
      (dired-up-directory)
      (kill-buffer old)))
  (defun yc/dired-create-file (file)
    (interactive
     (list
      (read-file-name "Create file: " (dired-current-directory))))
    (write-region "" nil (expand-file-name file) t)
    (dired-add-file file)
    (revert-buffer)
    (dired-goto-file (expand-file-name file)))

  ;; https://github.com/abo-abo/hydra/wiki/Dired
  (defhydra hydra-dired (:hint nil :color pink)
    "
    _+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
    _C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
    _D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
    _R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
    _Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
    _S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
    _r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
    _z_ compress-file  _A_ find regexp
    _Z_ compress       _Q_ repl regexp

    T - tag prefix
    "
    ("\\" dired-do-ispell)
    ("(" dired-hide-details-mode)
    (")" dired-omit-mode)
    ("+" dired-create-directory)
    ("=" diredp-ediff) ;; smart diff
    ("?" dired-summary)
    ("$" diredp-hide-subdir-nomove)
    ("A" dired-do-find-regexp)
    ("C" dired-do-copy) ;; Copy all marked files
    ("D" dired-do-delete)
    ("E" dired-mark-extension)
    ("e" dired-ediff-files)
    ("F" dired-do-find-marked-files)
    ("G" dired-do-chgrp)
    ("g" revert-buffer) ;; read all directories again (refresh)
    ("i" dired-maybe-insert-subdir)
    ("l" dired-do-redisplay) ;; relist the marked or singel directory
    ("M" dired-do-chmod)
    ("m" dired-mark)
    ("O" dired-display-file)
    ("o" dired-find-file-other-window)
    ("Q" dired-do-find-regexp-and-replace)
    ("R" dired-do-rename)
    ("r" dired-do-rsynch)
    ("S" dired-do-symlink)
    ("s" dired-sort-toggle-or-edit)
    ("t" dired-toggle-marks)
    ("U" dired-unmark-all-marks)
    ("u" dired-unmark)
    ("v" dired-view-file) ;; q to exit, s to search, = gets line #
    ("w" dired-kill-subdir)
    ("Y" dired-do-relsymlink)
    ("z" diredp-compress-this-file)
    ("Z" dired-do-compress)
    ("q" nil)
    ("." nil :color blue))
  (general-define-key :keymaps 'dired-mode-map
                      :states '(emacs)
                      "\\" 'hydra-dired/body)
  :general
  (yc/leader-keys-major-mode
    :keymaps 'dired-mode-map
    "DEL" 'my/dired-up-directory
    "RET" 'dired-find-alternate-file
    "TAB" 'dired-subtree-toggle
    "l" 'dired-find-alternate-file
    "c" 'dired-do-rename
    "C" 'dired-do-copy
    "y" 'dired-ranger-copy
    "p" 'dired-ranger-paste
    "v" 'dired-ranger-move
    "R" 'dired-do-redisplay
    "r" 'wdired-change-to-wdired-mode
    "f" 'counsel-file-jump
    "o" 'my/dired-create-file
    "O" 'dired-create-directory
    "q" 'kill-this-buffer
    "!" 'dired-do-shell-command))

(use-package wdired
  :after dired
  :commands wdired-change-to-wdired-mode
  :custom
  (wdired-allow-to-change-permissions t)
  :general
  (dired-mode-map
   "C-c C-c" 'wdired-change-to-wdired-mode))

;; dired 模式扩展
;; - https://github.com/Fuco1/dired-hacks
;; - https://jblevins.org/log/dired-open
(use-package direx
  :after dired
  :general
  ("C-x C-j" 'direx:jump-to-directory))
;; "z"       '(lambda () (interactive)
;;              (let ((fn (dired-get-file-for-visit)))
;;                (start-process "default-app" nil "open" fn)))))

;; (use-package dired-rainbow
;;   :init
;;   (eval-after-load 'dired '(require 'dired-rainbow))
;;   :config
;;   (dired-rainbow-define audio "#329EE8" ("mp3" "MP3" "ogg" "OGG" "flac" "FLAC" "wav" "WAV"))
;;   (dired-rainbow-define media "#ce5c00" ("mp3" "mp4" "MP3" "MP4" "avi" "mpg" "flv" "ogg"))
;;   (dired-rainbow-define video "#455AFC" ("vob" "VOB" "mkv" "MKV" "mpe" "mpg" "MPG" "mp4" "MP4" "ts" "TS" "m2ts"))
;;   (dired-rainbow-define html "#4e9a06" ("htm" "html" "xhtml"))
;;   (dired-rainbow-define xml "DarkGreen" ("xml" "xsd" "xsl" "xslt" "wsdl"))
;;   (dired-rainbow-define document "#ce5c00" ("doc" "docx" "odt" "pdb" "pdf" "ps" "rtf" "djvu"))
;;   (dired-rainbow-define image "#ff4b4b" ("jpg" "png" "jpeg" "gif"))
;;   (dired-rainbow-define sourcefile "#3F82FD" ("el" "groovy" "gradle" "py" "c" "cc" "h" "java" "pl" "rb"))
;;   (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
;;   (dired-rainbow-define compressed "#ad7fa8" ("zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
;;   (dired-rainbow-define packaged "#e6a8df" ("deb" "rpm"))
;;   (dired-rainbow-define encrypted "LightBlue" ("gpg" "pgp"))
;;   (dired-rainbow-define-chmod executable-unix "Green" "-.*x.*")
;;   (dired-rainbow-define log (:inherit default :italic t) ".*\\.log")
;;   (dired-rainbow-define org "#5F9EA0" (".*\\.org")))

;; (use-package dired-k
;;   :hook (dired-after-readin . dired-k-no-revert)
;;   :hook (dired-initial-position . dired-k)
;;   :config
;;   (setq dired-k-padding 1))

(use-package dired-hacks
  :load-path "localelpa/dired-hacks")

;; Show subtree when pressing =i=
(use-package dired-subtree
  :after (dired)
  :general
  (dired-mode-map
   "TAB" 'dired-subtree-toggle
   "<backtab>" 'dired-subtree-cycle))

(use-package dired-narrow
  :after (dired)
  :config
  (setq dired-narrow-exit-when-one-left t
        dired-narrow-enable-blinking t
        dired-narrow-blink-time 0.3)
  :general
  (dired-mode-map
   "C-c C-n" 'dired-narrow
   "C-c C-f" 'dired-narrow-fuzzy
   "C-c C-N" 'dired-narrow-regexp))

;; A package for viewing any list of files as a tree.
;; - https://github.com/knpatel401/filetree
(use-package filetree
  :disabled t
  :init (setq ;; filetree-notes-file "/home/david/Dropbox/Org/filtree-notes.org"
         filetree-info-window t
         filetree-use-all-the-icons t
         filetree-show-remote-file-info t)
  ;; :bind (("C-c f r" . filetree-show-recentf-files)
  ;;        ("C-c f f" . filetree-select-file-list)
  ;;        ("C-c f d" . filetree-show-cur-dir)
  ;;        ("C-c f D" . filetree-show-cur-dir-recursively)
  ;;        ("C-c f n" . filetree-show-files-with-notes))
  )

;; Peep file in dired
;; https://github.com/asok/peep-dired
(use-package peep-dired
  :custom
  (peep-dired-cleanup-on-disable t)
  (peep-dired-enable-on-directories t)
  (peep-dired-ignored-extensions '("mkv" "iso" "mp4"))
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'normal peep-dired-mode-map (kbd "<SPC>") 'peep-dired-scroll-page-down
                     (kbd "C-<SPC>") 'peep-dired-scroll-page-up
                     (kbd "<backspace>") 'peep-dired-scroll-page-up
                     (kbd "j") 'peep-dired-next-file
                     (kbd "k") 'peep-dired-prev-file)
    (add-hook 'peep-dired-hook 'evil-normalize-keymaps)))

;; Colourful columns.
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; adds git logs to dired file and directory details.
(use-package dired-git-info
  :after dired
  :config
  (setq dgi-commit-message-format "%h\t%s\t%cr")
  :bind (:map dired-mode-map
              (")" . dired-git-info-mode)))

(use-package all-the-icons-dired
  :after (dired all-the-icons)
  :hook (dired-mode . all-the-icons-dired-mode))

;; 类似 package
;; - dired-sidebar
;; ranger.el
(use-package dirvish
  :disabled t
  :init
  ;; Let Dirvish take over Dired globally
  (dirvish-override-dired-mode))

(use-package ranger
  :disabled t
  ;; :straight (:host github :repo "ralesi/ranger.el")
  )

;; Multi-stage copy/pasting of files and bookmarks
(use-package dired-ranger
  :disabled t
  :after (dired))

(provide 'init-dired)
