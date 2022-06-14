;; init-ivy.el --- Initialize ivy settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;;  ivy 是一个 Emacs 的补全框架，类似的有 ido 和 helm。
;; - https://github.com/abo-abo/swiper
;; 关于 ivy 的使用：https://writequit.org/denver-emacs/presentations/2017-04-11-ivy.html#fnr.1
;; - ido
;; - everything
;; - helm
;; - ivy
;; - snails
;; - selectrum + prescient
;; 

;;; Code
(require 'init-funcs)

(use-package flx  ;; Improves sorting for fuzzy-matched results
  :after ivy
  :init
  (setq ivy-flx-limit 10000))

(use-package ivy
  :demand t
  :general
  ("C-x b" 'ivy-switch-buffer
   "C-x B" 'ivy-switch-buffer-other-window)
  :bind
  (([remap switch-to-buffer] . ivy-switch-buffer)
   ([remap switch-to-bufer-other-window] . ivy-switch-buffer-other-window))
  :hook
  (minibuffer-setup . yc--minibuffer-setup-hook)
  (minibuffer-exit . yc--minibuffer-exit-hook)
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-selectable-prompt t)
  (ivy-display-style 'fancy)
  (ivy-use-virtual-buffers t)
  :config
  (ivy-mode 1)

  ;; @see - [[http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/][Why are you changing gc-cons-threshold?]]
  (defun yc--minibuffer-setup-hook ()
    (setq gc-cons-threshold most-positive-fixnum))

  (defun yc--minibuffer-exit-hook ()
    (setq gc-cons-threshold 16777216)))

;; https://github.com/Yevgnen/ivy-rich
;; https://github.com/casouri/ivy-filthy-rich

;; - https://gist.github.com/tam17aki/695995d23166683e7b4f04774e0eeda9
;; - https://github.com/walseb/QualityEmacsConfig
;; - https://github.com/11182711114/emacs-config/blob/master/emacsconfig.org
;; - https://github.com/mpereira/.emacs.d/blob/master/configuration.org
(use-package ivy-rich
  :demand t
  :hook ((after-init . ivy-rich-mode)
         (counsel-projectile-mode . ivy-rich-mode) ; MUST after `counsel-projectile'
         (ivy-rich-mode . (lambda ()
                            "Use abbreviate in `ivy-rich-mode'."
                            (setq ivy-virtual-abbreviate
                                  (or (and ivy-rich-mode 'abbreviate) 'name)))))
  :custom-face
  (ivy-current-match ((t (:extend t :background "gray98" :underline t))))
  :custom
  (ivy-rich-parse-remote-buffer nil)
  (ivy-rich-parse-remote-file-path nil)
  :init
  (setq ivy-rich-path-style 'abbreviate)
  (setq ivy-rich-display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-candidate (:width 30)) ; return the candidate itself
            (ivy-rich-switch-buffer-size (:width 7)) ; return the buffer size
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right)); return the buffer indicators
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))          ; return the major mode info
            (ivy-rich-switch-buffer-project (:width 30 :face success))             ; return project name using `projectile'
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))  ; return file path relative to project root or `default-directory' if project is nil
           :predicate
           (lambda (cand) (get-buffer cand)))
          ivy-switch-buffer-other-window
          (:columns
           ((ivy-rich-candidate (:width 30)) ; return the candidate itself
            (ivy-rich-switch-buffer-size (:width 7)) ; return the buffer size
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right)); return the buffer indicators
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))          ; return the major mode info
            (ivy-rich-switch-buffer-project (:width 30 :face success))             ; return project name using `projectile'
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))  ; return file path relative to project root or `default-directory' if project is nil
           :predicate
           (lambda (cand) (get-buffer cand)))
          counsel-find-file
          (:columns
           ((ivy-read-file-transformer)
            (ivy-rich-counsel-find-file-truename (:face font-lock-doc-face))))
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer (:width 60))  ; thr original transfomer
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return the docstring of the command
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer (:width 40))  ; the original transformer
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return the docstring of the function
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer (:width 40))  ; the original transformer
            (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))  ; return the docstring of the variable
          package-install
          (:columns
           ((ivy-rich-candidate (:width 30))
            (ivy-rich-package-version (:width 16 :face font-lock-comment-face))
            (ivy-rich-package-archive-summary (:width 7 :face font-lock-builtin-face))
            (ivy-rich-package-install-summary (:face font-lock-doc-face))))
          counsel-recentf
          (:columns
           ((ivy-rich-candidate (:width 0.8)) ; return the candidate itself
            (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))) ; return the last modified time of the file
        )
  :config
  (ivy-rich-project-root-cache-mode t)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package prescient
  :config
  (setq prescient-history-length 200)
  (setq prescient-save-file  (expand-file-name ".cache/prescient-items" user-emacs-directory))
  (setq prescient-filter-method '(literal regexp))
  (prescient-persist-mode 1))

(use-package ivy-prescient
  :after (prescient ivy)
  :hook (ivy-mode-hook . ivy-prescient-mode)
  :config
  (setq ivy-prescient-sort-commands
        '(:not counsel-grep
               counsel-rg
               counsel-switch-buffer
               ivy-switch-buffer
               swiper
               swiper-multi))
  (setq ivy-prescient-retain-classic-highlighting t)
  (setq ivy-prescient-enable-filtering nil)
  (setq ivy-prescient-enable-sorting t)
  (ivy-prescient-mode t))

;; https://github.com/abo-abo/hydra/wiki/hydra-ivy-replacement
(use-package ivy-hydra)

;; ;; (use-package ivy-posframe
;; ;;   :after (ivy swiper counsel)
;; ;;   :init
;; ;;   (progn
;; ;;     (push '(counsel-M-x . ivy-posframe-display-at-frame-center) ivy-display-functions-alist)
;; ;;     (push '(ivy-switch-buffer . ivy-posframe-display-at-frame-center) ivy-display-functions-alist)
;; ;;     (ivy-posframe-enable)
;; ;;     (set-face-attribute 'internal-border nil :background "gray50")
;; ;;     (setq ivy-posframe-hide-minibuffer nil)
;; ;;     (setq ivy-posframe-border-width 1)))

;; (use-package ivy-posframe
;;   :delight
;;   :hook (after-init . ivy-posframe-mode)
;;   :init
;;   (ivy-posframe-mode 1)
;;   :config
;;   (setq ivy-posframe-parameters
;;         '((left-fringe . 2)
;;           (right-fringe . 2)
;;           (internal-border-width . 2)
;;           ;; (font . "DejaVu Sans Mono-10.75:hintstyle=hintfull")
;;           ))
;;   ;; (setq ivy-posframe-height-alist
;;   ;;       '((swiper . 15)
;;   ;;         (swiper-isearch . 15)
;;   ;;         (t . 10)))
;;   (setq ivy-posframe-display-functions-alist
;;         '((complete-symbol . ivy-posframe-display-at-point)
;;           (swiper . nil)
;;           (swiper-isearch . nil)
;;           (t . ivy-posframe-display-at-frame-top-center))))

(use-package ivy-xref
  :after (ivy xref)
  :init
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; (use-package ivy-bibtex
;;   :commands ivy-bibtex
;;   :init
;;   (progn
;;     (yc/leader-keys
;; 	  "ib" 'ivy-bibtex)
;;     ;; (yc/leader-keys-minor-mode
;;     ;;   :keymaps 'bibtex-completion-notes-mode-map
;;     ;;   "s" 'bibtex-completion-exit-notes-buffer)
;;     )
;;   :config
;;   (setq bibtex-completion-pdf-field "file"
;; 	    bibtex-completion-cite-prompt-for-optional-arguments nil
;; 	    bibtex-completion-pdf-symbol "ρ"
;; 	    bibtex-completion-notes-symbol "η"
;; 	    bibtex-completion-find-additional-pdfs t
;; 	    bibtex-completion-bibliography "~/Dropbox/itsycnotes/bibtex/main.bib"
;; 	    bibtex-completion-notes-path "~/Dropbox/itsycnotes/references/ref-notes.org"
;; 	    bibtex-completion-notes-template-one-file
;; 	    "\n** ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :NOTER_DOCUMENT: ${file}\n  :END:"
;; 	    bibtex-completion-additional-search-fields '(eventtitle)
;; 	    bibtex-completion-display-formats
;; 	    '((t . "${=has-pdf=:1} ${=has-note=:1} ${=type=:3} ${year:4} ${author:25} ${title:100}"))))

;;  类似于 =M-x isearch= 的功能，提供内容查找功能
;; - https://github.com/abo-abo/swiper
;; - https://oremacs.com/swiper/
(use-package swiper
  :commands (swiper swiper-all swiper-isearch)
  :general
  ("C-M-s" 'swiper-isearch
   "M-s s" 'swiper-isearch
   "C-s" 'swiper-isearch
   "C-c u" 'swiper-all)
  (swiper-map
   "M-q" 'swiper-query-replace
   "C-l" 'swiper-recenter-top-bottom
   "C-." 'swiper-mc
   "C-'" 'swiper-avy))

;; Display an arrow with the selected item
(defun my-ivy-format-function-arrow (cands)
  "Transform CANDS into a string for minibuffer."
  (ivy--format-function-generic
   (lambda (str)
     (concat (if (and (bound-and-true-p all-the-icons-ivy-rich-mode)
                      (>= (length str) 1)
                      (string= " " (substring str 0 1)))
                 ">"
               "> ")
             (ivy--add-face str 'ivy-current-match)))
   (lambda (str)
     (concat (if (and (bound-and-true-p all-the-icons-ivy-rich-mode)
                      (>= (length str) 1)
                      (string= " " (substring str 0 1)))
                 " "
               "  ")
             str))
   cands
   "\n"))
(setf (alist-get 't ivy-format-functions-alist) #'my-ivy-format-function-arrow)

;; Pre-fill search keywords
;; @see https://www.reddit.com/r/emacs/comments/b7g1px/withemacs_execute_commands_like_marty_mcfly/
(defvar my-ivy-fly-commands
  '(query-replace-regexp
    flush-lines keep-lines ivy-read
    swiper swiper-backward swiper-all
    swiper-isearch swiper-isearch-backward
    lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol
    counsel-grep-or-swiper counsel-grep-or-swiper-backward
    counsel-grep counsel-ack counsel-ag counsel-rg counsel-pt))
(defvar-local my-ivy-fly--travel nil)

(defun my-ivy-fly-back-to-present ()
  (cond ((and (memq last-command my-ivy-fly-commands)
              (equal (this-command-keys-vector) (kbd "M-p")))
         ;; repeat one time to get straight to the first history item
         (setq unread-command-events
               (append unread-command-events
                       (listify-key-sequence (kbd "M-p")))))
        ((or (memq this-command '(self-insert-command
                                  ivy-forward-char
                                  ivy-delete-char delete-forward-char
                                  end-of-line mwim-end-of-line
                                  mwim-end-of-code-or-line mwim-end-of-line-or-code
                                  yank ivy-yank-word counsel-yank-pop))
             (equal (this-command-keys-vector) (kbd "M-n")))
         (unless my-ivy-fly--travel
           (delete-region (point) (point-max))
           (when (memq this-command '(ivy-forward-char
                                      ivy-delete-char delete-forward-char
                                      end-of-line mwim-end-of-line
                                      mwim-end-of-code-or-line
                                      mwim-end-of-line-or-code))
             (insert (ivy-cleanup-string ivy-text))
             (when (memq this-command '(ivy-delete-char delete-forward-char))
               (beginning-of-line)))
           (setq my-ivy-fly--travel t)))))

(defun my-ivy-fly-time-travel ()
  (when (memq this-command my-ivy-fly-commands)
    (let* ((kbd (kbd "M-n"))
           (cmd (key-binding kbd))
           (future (and cmd
                        (with-temp-buffer
                          (when (ignore-errors
                                  (call-interactively cmd) t)
                            (buffer-string))))))
      (when future
        (save-excursion
          (insert (propertize (replace-regexp-in-string
                               "\\\\_<" ""
                               (replace-regexp-in-string
                                "\\\\_>" ""
                                future))
                              'face 'shadow)))
        (add-hook 'pre-command-hook 'my-ivy-fly-back-to-present nil t)))))

(add-hook 'minibuffer-setup-hook #'my-ivy-fly-time-travel)
(add-hook 'minibuffer-exit-hook
          (lambda ()
            (remove-hook 'pre-command-hook 'my-ivy-fly-back-to-present t)))

;; (defun maple/mac-switch-input-source ()
;;   "docstring..."
;;   (interactive)
;;   (shell-command
;;    "osascript -e 'tell application \"System Events\" to tell process \"SystemUIServer\"
;;     set currentLayout to get the value of the first menu bar item of menu bar 1 whose description is \"text input\"
;;     if currentLayout is not \"U.S.\" then
;;       tell (1st menu bar item of menu bar 1 whose description is \"text input\") to {click, click (menu 1'\"'\"'s menu item \"U.S.\")}
;;     end if
;;   end tell' &>/dev/null"))

;; ;; (add-hook 'focus-in-hook 'maple/mac-switch-input-source)

;; (defun yc/custom-M-x ()
;;   ""
;;   (interactive)
;;   (let
;;     (maple/mac-switch-input-source)
;;     (counsel-M-x)))

(use-package counsel
  :demand t
  :ensure-system-package
  ((ag . "brew install the_silver_searcher")
   (rg . "brew install ripgrep"))
  :config
  (setq counsel-yank-pop-preselect-last t)
  (setq counsel-find-file-at-point t)
  (setq counsel-yank-pop-separator "\n—————————\n")
  (if (executable-find "rg")
      ;; if rg is installed, use rg for `counsel-grep-or-swiper' and `counsel-rg'
      (setq counsel-grep-base-command "rg -n -M 512 --line-number --smart-case --with-filename --color never --no-heading -i \"%s\" %s"
            ;; add `--follow' option to allow search through symbolic links
            ;; ripgrep 的文档说明中文版：https://github.com/chinanf-boy/ripgrep-zh
            counsel-rg-base-command "rg -SHn -M 512 --line-number --smart-case --with-filename --color never --no-follow --no-heading %s"
            ;; Use ripgrep for counsel-git
            counsel-git-cmd "rg --files")
    ;; ignore case sensitivity for counsel grep
    (setq counsel-grep-base-command "grep -nEi \"%s\" %s"))
  ;; (setq counsel-grep-base-command
  ;;       (concat (executable-find "rg")
  ;;               " -n -M 512 --no-heading --color never -i \"%s\" %s"))
  ;; (setq counsel-rg-base-command
  ;;       "rg -SHn --no-heading --color never --no-follow --hidden %s")
  (setq counsel-find-file-occur-use-find nil)
  (setq counsel-find-file-occur-cmd; TODO Simplify this
        "gls -a | grep -i -E '%s' | tr '\\n' '\\0' | xargs -0 ls -d --dired-listing-switches")
  
  (defun prot/counsel-fzf-rg-files (&optional input dir)
    "Run `fzf' in tandem with `ripgrep' to find files in the
  present directory.  If invoked from inside a version-controlled
  repository, then the corresponding root is used instead."
    (interactive)
    (let* ((process-environment
            (cons (concat "FZF_DEFAULT_COMMAND=rg -Sn --color never --files --no-follow --hidden")
                  process-environment))
           (vc (vc-root-dir)))
      (if dir
          (counsel-fzf input dir)
        (if (eq vc nil)
            (counsel-fzf input default-directory)
          (counsel-fzf input vc)))))
  
  (defun prot/counsel-fzf-dir (arg)
    "Specify root directory for `counsel-fzf'."
    (prot/counsel-fzf-rg-files ivy-text
                               (read-directory-name
                                (concat (car (split-string counsel-fzf-cmd))
                                        " in directory: "))))
  
  (defun prot/counsel-rg-dir (arg)
    "Specify root directory for `counsel-rg'."
    (let ((current-prefix-arg '(4)))
      (counsel-rg ivy-text nil "")))
  
  ;; TODO generalise for all relevant file/buffer counsel-*?
  (defun prot/counsel-fzf-ace-window (arg)
    "Use `ace-window' on `prot/counsel-fzf-rg-files' candidate."
    (ace-window t)
    (let ((default-directory (if (eq (vc-root-dir) nil)
                                 counsel--fzf-dir
                               (vc-root-dir))))
      (if (> (length (aw-window-list)) 1)
          (find-file arg)
        (find-file-other-window arg))
      (balance-windows (current-buffer))))
  
  ;;
  ;; Improve search experience of `swiper' and `counsel'
  ;;
  (defun my-ivy-switch-to-swiper (&rest _)
    "Switch to `swiper' with the current input."
    (swiper ivy-text))
  
  (defun my-ivy-switch-to-swiper-isearch (&rest _)
    "Switch to `swiper-isearch' with the current input."
    (swiper-isearch ivy-text))
  
  (defun my-ivy-switch-to-swiper-all (&rest _)
    "Switch to `swiper-all' with the current input."
    (swiper-all ivy-text))
  
  (defun my-ivy-switch-to-rg-dwim (&rest _)
    "Switch to `rg-dwim' with the current input."
    (rg-dwim default-directory))
  
  (defun my-ivy-switch-to-counsel-rg (&rest _)
    "Switch to `counsel-rg' with the current input."
    (counsel-rg ivy-text default-directory))
  
  (defun my-ivy-switch-to-counsel-git-grep (&rest _)
    "Switch to `counsel-git-grep' with the current input."
    (counsel-git-grep ivy-text default-directory))
  
  (defun my-ivy-switch-to-counsel-find-file (&rest _)
    "Switch to `counsel-find-file' with the current input."
    (counsel-find-file ivy-text))
  
  (defun my-ivy-switch-to-counsel-fzf (&rest _)
    "Switch to `counsel-fzf' with the current input."
    (counsel-fzf ivy-text default-directory))
  
  (defun my-ivy-switch-to-counsel-git (&rest _)
    "Switch to `counsel-git' with the current input."
    (counsel-git ivy-text))
  
  ;; @see https://emacs-china.org/t/swiper-swiper-isearch/9007/12
  (defun my-swiper-toggle-counsel-rg ()
    "Toggle `counsel-rg' and `swiper'/`swiper-isearch' with the current input."
    (interactive)
    (ivy-quit-and-run
      (if (memq (ivy-state-caller ivy-last) '(swiper swiper-isearch))
          (my-ivy-switch-to-counsel-rg)
        (my-ivy-switch-to-swiper-isearch))))
  (bind-key "<C-return>" #'my-swiper-toggle-counsel-rg swiper-map)
  (bind-key "<C-return>" #'my-swiper-toggle-counsel-rg counsel-ag-map)
  
  (with-eval-after-load 'rg
    (defun my-swiper-toggle-rg-dwim ()
      "Toggle `rg-dwim' with the current input."
      (interactive)
      (ivy-quit-and-run
        (rg-dwim default-directory)))
    (bind-key "<M-return>" #'my-swiper-toggle-rg-dwim swiper-map)
    (bind-key "<M-return>" #'my-swiper-toggle-rg-dwim counsel-ag-map))
  
  (defun my-swiper-toggle-swiper-isearch ()
    "Toggle `swiper' and `swiper-isearch' with the current input."
    (interactive)
    (ivy-quit-and-run
      (if (eq (ivy-state-caller ivy-last) 'swiper-isearch)
          (swiper ivy-text)
        (swiper-isearch ivy-text))))
  (bind-key "<s-return>" #'my-swiper-toggle-swiper-isearch swiper-map)
  
  (defun my-counsel-find-file-toggle-fzf ()
    "Toggle `counsel-fzf' with the current `counsel-find-file' input."
    (interactive)
    (ivy-quit-and-run
      (counsel-fzf (or ivy-text "") default-directory)))
  (bind-key "<C-return>" #'my-counsel-find-file-toggle-fzf counsel-find-file-map)
  
  (defun my-swiper-toggle-rg-dwim ()
    "Toggle `rg-dwim' with the current input."
    (interactive)
    (ivy-quit-and-run (my-ivy-switch-to-rg-dwim)))
  (bind-key "<M-return>" #'my-swiper-toggle-rg-dwim swiper-map)
  (bind-key "<M-return>" #'my-swiper-toggle-rg-dwim counsel-ag-map)
  
  (defun my-swiper-toggle-swiper-isearch ()
    "Toggle `swiper' and `swiper-isearch' with the current input."
    (interactive)
    (ivy-quit-and-run
      (if (eq (ivy-state-caller ivy-last) 'swiper-isearch)
          (my-ivy-switch-to-swiper)
        (my-ivy-switch-to-swiper-isearch))))
  (bind-key "<s-return>" #'my-swiper-toggle-swiper-isearch swiper-map)
  
  (ivy-add-actions
   'ivy-switch-buffer
   '(("r" my-ivy-switch-to-counsel-rg "rg")
     ("d" my-ivy-switch-to-rg-dwim "rg dwim")
     ("g" my-ivy-switch-to-counsel-git "git")
     ("z" my-ivy-switch-to-counsel-fzf "fzf")
     ("G" prot/counsel-rg-dir "use ripgrep in root directory")
     ("Z" prot/counsel-fzf-dir "find file with fzf in root directory")
     ))
  ;; Pass functions as appropriate Ivy actions (accessed via M-o)
  (ivy-add-actions
   'swiper-isearch
   '(("r" my-ivy-switch-to-counsel-rg "rg")
     ("d" my-ivy-switch-to-rg-dwim "rg dwim")
     ("s" my-ivy-switch-to-swiper "swiper")
     ("a" my-ivy-switch-to-swiper-all "swiper all")))
  
  (ivy-add-actions
   'swiper
   '(("r" my-ivy-switch-to-counsel-rg "rg")
     ("d" my-ivy-switch-to-rg-dwim "rg dwim")
     ("s" my-ivy-switch-to-swiper-isearch "swiper isearch")
     ("a" my-ivy-switch-to-swiper-all "swiper all")))
  
  (ivy-add-actions
   'swiper-all
   '(("g" my-ivy-switch-to-counsel-git-grep "git grep")
     ("r" my-ivy-switch-to-counsel-rg "rg")
     ("d" my-ivy-switch-to-rg-dwim "rg dwim")
     ("s" my-swiper-toggle-swiper-isearch "swiper isearch")
     ("S" my-ivy-switch-to-swiper "swiper")))
  
  (ivy-add-actions
   'counsel-git-grep
   '(("s" my-ivy-switch-to-swiper-isearch "swiper isearch")
     ("S" my-ivy-switch-to-swiper "swiper")
     ("r" my-ivy-switch-to-rg-dwim "rg")
     ("d" my-ivy-switch-to-rg-dwim "rg dwim")
     ("a" my-ivy-switch-to-swiper-all "swiper all")))
  
  (ivy-add-actions
   'counsel-find-file
   '(("g" my-ivy-switch-to-counsel-git "git")
     ("z" my-ivy-switch-to-counsel-fzf "fzf")
     ("G" prot/counsel-rg-dir "use ripgrep in root directory")
     ("Z" prot/counsel-fzf-dir "find file with fzf in root directory")
     ("l" vlf "view large file (vlf)")))
  
  (ivy-add-actions
   'counsel-fzf
   '(("r" prot/counsel-fzf-dir "change root directory")
     ("f" my-ivy-switch-to-counsel-find-file "find file")
     ("g" my-ivy-switch-to-counsel-git "git")
     ("G" prot/counsel-rg-dir "use ripgrep in root directory")
     ("a" prot/counsel-fzf-ace-window "ace-window switch")))
  
  (ivy-add-actions
   'counsel-rg
   '(("s" my-ivy-switch-to-swiper-isearch "swiper isearch")
     ("S" my-ivy-switch-to-swiper "swiper")
     ("a" my-ivy-switch-to-swiper-all "swiper all")
     ("d" my-ivy-switch-to-rg-dwim "rg dwim")
     ("r" prot/counsel-rg-dir "change root directory")
     ("z" prot/counsel-fzf-dir "find file with fzf in root directory")))
  
  (ivy-add-actions
   'counsel-git
   '(("f" my-ivy-switch-to-counsel-find-file "find file")
     ("z" my-ivy-switch-to-counsel-fzf "fzf")))
  
  (ivy-add-actions
   'counsel-projectile-find-file
   '(("f" my-ivy-switch-to-counsel-find-file "find file")
     ("d" my-ivy-switch-to-rg-dwim "rg dwim")
     ("R" my-ivy-switch-to-counsel-rg "rg")
     ("g" my-ivy-switch-to-counsel-git "git")
     ("G" prot/counsel-rg-dir "use ripgrep in root directory")
     ("z" my-ivy-switch-to-counsel-fzf "fzf")))
  
  (ivy-add-actions
   'counsel-org-goto
   '(("s" my-ivy-switch-to-swiper-isearch "swiper isearch")
     ("S" my-ivy-switch-to-swiper "swiper")
     ("a" my-ivy-switch-to-swiper-all "swiper all")))
  :general
  ("C-x C-f" 'counsel-find-file
             "C-x d"   'counsel-dired
             "M-x"     'counsel-M-x
             "M-y"     'counsel-yank-pop
             "M-s M-r" 'counsel-rg
             "M-s r"   'rg
             "M-s R"   'rg-project
             "M-s a"   'counsel-ag
             "M-s g"   'counsel-git-grep
             "M-s f"   'counsel-fzf
             "M-s F"   'fiplr-find-file
             "C-c C-r" 'ivy-resume
             "C-c i"   'counsel-imenu
             "C-x k"   'kill-buffer
             "C-x l"   'counsel-locate
             "C-h f"   'counsel-describe-function
             "C-h v"   'counsel-describe-variable
             ;; "C-c j"   'counsel-git    ; 与 org-journal 冲突
             "C-c f"   'counsel-recentf)
  (help-map
   "f" 'counsel-describe-function
   "v" 'counsel-describe-variable
   "l" 'counsel-info-lookup-symbol))

;; https://github.com/ericdanan/counsel-projectile
(use-package counsel-projectile
  :after counsel
  :hook (counsel-mode . counsel-projectile-mode)
  :init (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point))
  :general ("C-x C-p" 'counsel-projectile-switch-project
            "C-x p B" 'counsel-projectile-switch-to-buffer))

(use-package counsel-osx-app
  :after counsel
  ;; :general (("s-o" 'counsel-osx-app))
  )

(use-package counsel-fd
  :after counsel
  :config
  (defalias 'fd 'counsel-fd-file-jump))

(use-package counsel-at-point
  :commands
  (counsel-at-point-file-jump
   counsel-at-point-git-grep
   counsel-at-point-imenu)
  ;; :general
  ;; Example key bindings.
  ;; (("M-n" 'counsel-at-point-git-grep)
  ;; ("M-p" 'counsel-at-point-file-jump)
  ;; ("C-c i" 'counsel-at-point-imenu)
  ;; )
  )

;; - https://github.com/redguardtoo/counsel-etags
;; (use-package counsel-etags
;;   :general
;;   (yc/nonprefix-keys
;;     "C-]" 'counsel-etags-find-tag-at-point)
;;   :init
;;   (add-hook 'prog-mode-hook
;;             (lambda ()
;;               (add-hook 'after-save-hook
;;                         'counsel-etags-virtual-update-tags 'append 'local)))
;;   :config
;;   (setq counsel-etags-update-interval 60)
;;   (push "build" counsel-etags-ignore-directories))

(use-package ivy-fuz
  :disabled t
  :demand t
  :after ivy
  :custom
  (ivy-sort-matches-functions-alist '((t . ivy-fuz-sort-fn)))
  (ivy-re-builders-alist '((t . ivy-fuz-regex-fuzzy)))
  :config
  (add-to-list 'ivy-highlight-functions-alist '(ivy-fuz-regex-fuzzy . ivy-fuz-highlight-fn)))

(provide 'init-ivy)
