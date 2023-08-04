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
  (dired-use-ls-dired 'unspecified)
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
  :mode-hydra
  ;; https://github.com/abo-abo/hydra/wiki/Dired
  (dired-mode
   (:title "Dired Mode" :color pink :separator "═")
   ("Edit"
    (("+" dired-create-directory "mkdir")
     ("C" dired-do-copy "copy") ;; Copy all marked files
     ("D" dired-do-delete "delete")
     ("R" dired-do-rename "rename")
     ("Y" dired-do-relsymlink "rel symlink")
     ("S" dired-do-symlink "symlink")
     ("r" dired-do-rsynch "rsync")
     ("z" diredp-compress-this-file "compress file")
     ("Z" dired-do-compress "compress"))
    "View"
    (("v" dired-view-file "view") ;; q to exit, s to search, = gets line #
     ("O" dired-display-file "view other")
     ("o" dired-find-file-other-window "open other")
     ("M" dired-do-chmod "chmod")
     ("G" dired-do-chgrp "chgrp")
     ("A" dired-do-find-regexp "find regexp")
     ("Q" dired-do-find-regexp-and-replace "repl regexp"))
    "Mark"
    (("m" dired-mark "mark")
     ("U" dired-unmark-all-marks "mark all")
     ("u" dired-unmark "unmark")
     ("t" dired-toggle-marks "toggle mark")
     ("E" dired-mark-extension "extend mark")
     ("F" dired-do-find-marked-files "find mark"))
    "Info"
    (("(" dired-hide-details-mode "details")
     (")" dired-omit-mode "omit mode")
     ("l" dired-do-redisplay "redisplay") ;; relist the marked or singel directory
     ("g" revert-buffer "revert buffer") ;; read all directories again (refresh)
     ("s" dired-sort-toggle-or-edit "sort")
     ("." nil "toggle hydra" :color blue))
    "Directory"
    (("i" dired-maybe-insert-subdir "insert subdir")
     ("$" diredp-hide-subdir-nomove "hide subdir")
     ("w" dired-kill-subdir "kill subdir")
     ("e" dired-ediff-files "ediff")
     ("=" diredp-ediff "pdiff") ;; smart diff
     ("\\" dired-do-ispell "flyspell")
     ("?" dired-summary "summary"))
    ;; "wdired"
    ))
  ;; :general
  ;; (dired-mode-map
  ;;  "C-h h" 'hydra-dired/body)
  ;; :general
  ;; (yc/leader-keys-major-mode
  ;;   :keymaps 'dired-mode-map
  ;;   "DEL" 'my/dired-up-directory
  ;;   "RET" 'dired-find-alternate-file
  ;;   "TAB" 'dired-subtree-toggle
  ;;   "l" 'dired-find-alternate-file
  ;;   "c" 'dired-do-rename
  ;;   "C" 'dired-do-copy
  ;;   "y" 'dired-ranger-copy
  ;;   "p" 'dired-ranger-paste
  ;;   "v" 'dired-ranger-move
  ;;   "R" 'dired-do-redisplay
  ;;   "r" 'wdired-change-to-wdired-mode
  ;;   "f" 'counsel-file-jump
  ;;   "o" 'my/dired-create-file
  ;;   "O" 'dired-create-directory
  ;;   "q" 'kill-this-buffer
  ;;   "!" 'dired-do-shell-command)
  )

;; Writeable dired
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
  (dired-mode-map
   "C-x C-j" 'direx:jump-to-directory))
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

;; Peep file in dired
;; https://github.com/asok/peep-dired
(use-package peep-dired
  :custom
  (peep-dired-cleanup-on-disable t)
  (peep-dired-enable-on-directories t)
  (peep-dired-ignored-extensions '("mkv" "iso" "mp4"))
  ;; :config
  ;; (with-eval-after-load 'evil
  ;;   (evil-define-key 'normal peep-dired-mode-map (kbd "<SPC>") 'peep-dired-scroll-page-down
  ;;                    (kbd "C-<SPC>") 'peep-dired-scroll-page-up
  ;;                    (kbd "<backspace>") 'peep-dired-scroll-page-up
  ;;                    (kbd "j") 'peep-dired-next-file
  ;;                    (kbd "k") 'peep-dired-prev-file)
  ;;   (add-hook 'peep-dired-hook 'evil-normalize-keymaps))
  )

;; Colourful columns.
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; adds git logs to dired file and directory details.
(use-package dired-git-info
  :after dired
  :config
  (setq dgi-commit-message-format "%h\t%s\t%cr")
  :general
  (dired-mode-map
   ")" 'dired-git-info-mode))

(use-package all-the-icons-dired
  :disabled t
  :after (dired all-the-icons)
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; 类似 package
;; - dired-sidebar
;; - ranger.el
;; A package for viewing any list of files as a tree.
;; - https://github.com/knpatel401/filetree
(use-package filetree
  :disabled t
  :init (setq
         ;; filetree-notes-file "/home/david/Dropbox/Org/filtree-notes.org"
         filetree-info-window t
         filetree-use-all-the-icons t
         filetree-show-remote-file-info t)
  ;; :bind (("C-c f r" . filetree-show-recentf-files)
  ;;        ("C-c f f" . filetree-select-file-list)
  ;;        ("C-c f d" . filetree-show-cur-dir)
  ;;        ("C-c f D" . filetree-show-cur-dir-recursively)
  ;;        ("C-c f n" . filetree-show-files-with-notes))
  )

(use-package dirvish
  :disabled t
  :init
  ;; Let Dirvish take over Dired globally
  (dirvish-override-dired-mode))

(use-package ranger
  :disabled t)

;; Multi-stage copy/pasting of files and bookmarks
(use-package dired-ranger
  :disabled t
  :after (dired))

(provide 'init-dired)
