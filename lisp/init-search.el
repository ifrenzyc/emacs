;; init-search.el --- Initialize search configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:
(require 'init-embark)

(use-package isearch
  :ensure nil
  :no-require t
  :bind
  (
   ;; "C-s"     . isearch-forward-regexp
   ("C-r"       . isearch-backward-regexp)
   ("C-M-s"     . isearch-forward-other-window)
   ("C-M-r"     . isearch-backward-other-window)
   ("M-s ."     . isearch-forward-symbol-at-point)
   ("M-s <SPC>" . xah-search-current-word))
  (:map isearch-mode-map
        ("C-;" . consult-line)
        ("C-'" . avy-isearch)
        ("C-l" . consult-git-grep))
  :config
  ;; Isearch in other windows
  (defun isearch-forward-other-window (prefix)
    "Function to isearch-forward in other-window."
    (interactive "P")
    (unless (one-window-p)
      (save-excursion
        (let ((next (if prefix -1 1)))
          (other-window next)
          (isearch-forward)
          (other-window (- next))))))

  (defun isearch-backward-other-window (prefix)
    "Function to isearch-backward in other-window."
    (interactive "P")
    (unless (one-window-p)
      (save-excursion
        (let ((next (if prefix 1 -1)))
          (other-window next)
          (isearch-backward)
          (other-window (- next))))))

  (defun xah-search-current-word ()
    "Call `isearch' on current word or text selection.
“word” here is A to Z, a to z, and hyphen 「-」 and underline 「_」, independent of syntax table.
URL `http://ergoemacs.org/emacs/modernization_isearch.html'
Version 2015-04-09"
    (interactive)
    (let ( $p1 $p2 )
      (if (use-region-p)
          (progn
            (setq $p1 (region-beginning))
            (setq $p2 (region-end)))
        (save-excursion
          (skip-chars-backward "-_A-Za-z0-9")
          (setq $p1 (point))
          (right-char)
          (skip-chars-forward "-_A-Za-z0-9")
          (setq $p2 (point))))
      (setq mark-active nil)
      (when (< $p1 (point))
        (goto-char $p1))
      (isearch-mode t)
      (isearch-yank-string (buffer-substring-no-properties $p1 $p2)))))

;; 快速在当前 buffer 中跳转光标
;; - avy
;; - ace-jump-mode
;; - evil-easymotion
;; - https://github.com/abo-abo/avy
;; - http://ergoemacs.org/misc/ace_jump_avy_vs_isearch.html
;; - 配置来源于： https://gist.github.com/karthink/af013ffd77fe09e67360f040b57b4c7b （https://karthinks.com/software/avy-can-do-anything/）
;; - https://gist.github.com/Zetagon/1068474ef68ae0640f14dda240966ad1
(use-package avy
  :bind
  (("C-;"   . avy-goto-char-timer)
   ("C-:"   . avy-goto-line)
   ("M-g A" . ace-jump-two-chars-mode)
   ("M-g a" . avy-goto-char)
   ("M-g l" . avy-goto-char-2))
  ;; (:map isearch-mode-map
  ;;       ("M-j" . avy-isearch))
  ;; (swiper-map
  ;;   "M-j" 'swiper-avy)
  :custom
  ((avy-all-windows nil)
   (avy-all-windows-alt t)
   (avy-background t)
   (avy-style 'pre)
   (avy-keys '(?q ?e ?r ?y ?u ?o ?p
                  ?a ?s ?d ?f ?g ?h ?j
                  ?k ?l ?' ?x ?c ?v ?b
                  ?n ?, ?/)))
  :hook
  (after-init . avy-setup-default)
  :config
  (defhydra hydra-avy (:color red)
    "avy-goto"
    ("c" avy-goto-char "char")
    ("C" avy-goto-char-2 "char-2")
    ("w" avy-goto-word-1 "word")
    ("l" avy-goto-line "line")
    ("s" avy-goto-subword-1 "subword")
    ("u" link-hint-open-link "open-URI")
    ("U" link-hint-copy-link "copy-URI"))

  (defun avy-show-dispatch-help ()
    (let* ((len (length "avy-action-"))
           (fw (frame-width))
           (raw-strings (mapcar
                         (lambda (x)
                           (format "%2s: %-19s"
                                   (propertize
                                    (char-to-string (car x))
                                    'face 'aw-key-face)
                                   (substring (symbol-name (cdr x)) len)))
                         avy-dispatch-alist))
           (max-len (1+ (apply #'max (mapcar #'length raw-strings))))
           (strings-len (length raw-strings))
           (per-row (floor fw max-len))
           display-strings)
      (cl-loop for string in raw-strings
               for N from 1 to strings-len do
               (push (concat string " ") display-strings)
               (when (= (mod N per-row) 0) (push "\n" display-strings)))
      (message "%s" (apply #'concat (nreverse display-strings)))))

  ;; Kill text
  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (setf (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
        (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line)

  ;; Copy text
  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (setf (alist-get ?w avy-dispatch-alist) 'avy-action-copy
        (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line)

  ;; Yank text
  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)

  (setf (alist-get ?y avy-dispatch-alist) 'avy-action-yank
        (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line)

  ;; Transpose/Move text
  (defun avy-action-teleport-whole-line (pt)
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)

  (setf (alist-get ?t avy-dispatch-alist) 'avy-action-teleport
        (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line)

  ;; Mark text
  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))

  (setf (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char)

  ;; Flyspell words
  (defun avy-action-flyspell (pt)
    (save-excursion
      (goto-char pt)
      (when (require 'flyspell nil t)
        (flyspell-auto-correct-word)))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  ;; Bind to semicolon (flyspell uses C-;)
  (setf (alist-get ?\; avy-dispatch-alist) 'avy-action-flyspell)

  ;; Dictionary: define words
  ;; Replace your package manager or preferred dict package
  ;; (package-install 'dictionary)

  (defun dictionary-search-dwim (&optional arg)
    "Search for definition of word at point. If region is active,
search for contents of region instead. If called with a prefix
argument, query for word to search."
    (interactive "P")
    (if arg
        (dictionary-search nil)
      (if (use-region-p)
          (dictionary-search (buffer-substring-no-properties
                              (region-beginning)
                              (region-end)))
        (if (thing-at-point 'word)
            (dictionary-lookup-definition)
          (dictionary-search-dwim '(4))))))

  (defun avy-action-define (pt)
    (save-excursion
      (goto-char pt)
      (dictionary-search-dwim))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (setf (alist-get ?= avy-dispatch-alist) 'dictionary-search-dwim)

  ;; Get Elisp Help
  ;; Replace with your package manager or help library of choice
  ;; (package-install 'helpful)

  (defun avy-action-helpful (pt)
    (save-excursion
      (goto-char pt)
      (helpful-at-point))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (setf (alist-get ?H avy-dispatch-alist) 'avy-action-helpful)

  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark)

  ;; Google search: requires executable Tuxi
  (defvar google-search-history nil
    "List of queries to google-search-string.")
  (defun google-search-string (search-string)
    "Read SEARCH-STRING from the minibuffer and call the shell
command tuxi on it."
    (interactive (list (read-string "Google: " nil
                                    google-search-history
                                    (thing-at-point 'sexp))))
    (unless (executable-find "tuxi")
      (user-error "Cannot find shell command: tuxi"))
    (let ((search-output (string-trim-right
                          (shell-command-to-string
                           (concat
                            "tuxi -r "
                            (shell-quote-argument search-string))))))
      (with-current-buffer (get-buffer-create "*Tuxi Output*")
        (erase-buffer)
        (insert search-output)
        ;; (fill-region (point-min) (point-max))
        (if (<= (count-lines (point-min) (point-max)) 1)
            (message search-output)
          (goto-char (point-min))
          (display-buffer (current-buffer))
          (goto-address-mode 1)))))
  (defun google-search-at-point (&optional beg end)
    "Call the shell command tuxi on the symbol at point. With an
active region use it instead."
    (interactive "r")
    (if-let ((search-string (if (use-region-p)
                                (buffer-substring-no-properties beg end)
                              (thing-at-point 'symbol))))
        (google-search-string search-string)
      ;; (message "No symbol to search for at point!")
      (call-interactively #'google-search-string)))

  (defun avy-action-tuxi (pt)
    (cl-letf (((symbol-function 'keyboard-quit)
               #'abort-recursive-edit))
      (save-excursion
        (goto-char pt)
        (google-search-at-point))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (setf (alist-get ?G avy-dispatch-alist) 'avy-action-tuxi))

(use-package fzf)

;; 基于 Silver Searcher 的多文件内容查找
;; - https://github.com/Wilfred/ag.el
(use-package ag
  :ensure-system-package (ag . "brew install the_silver_searcher"))

(use-package rg
  :custom
  (rg-group-result t)
  (rg-show-header t))

;; https://github.com/manateelazycat/color-rg
(use-package color-rg
  :ensure-system-package (rg . "brew install ripgrep")
  :load-path "localelpa/color-rg"
  :bind
  ("M-s M-s" . color-rg-search-project)
  :commands (color-rg-mode
             color-rg-search-input
             color-rg-search-symbol
             color-rg-search-project))

;; *About:* 多文件查找替换
;; - https://github.com/mhayashi1120/Emacs-wgrep
;; - https://www.reddit.com/r/emacs/comments/ce0557/emacs_editing_multiple_files_with_grep_wgrep/
;; Writeable grep
(use-package wgrep
  :commands (wgrep-change-to-wgrep-mode ivy-wgrep-change-to-wgrep-mode)
  :config
  (setq wgrep-enable-key "r"
        wgrep-auto-save-buffer t))

(use-package ripgrep :commands ripgrep-regexp)

(use-package find-file-rg
  :bind
  ("C-c f"   . find-file-rg)
  ("C-c g"   . find-file-rg-at-point))

(use-package deadgrep)

;;================================================================================
(use-package loccur
  :disabled t
  :commands (loccur-isearch loccur-current loccur))

(use-package occur
  :disabled t
  :ensure nil
  :no-require t
  :bind
  ("M-s o" . occur-dwim)
  :config
  ;; - https://github.com/wandersoncferreira/dotfiles/blob/master/README.org#occur
  (defun occur-dwim ()
    "Call `occur' with a sane default."
    (interactive)
    (push (if (region-active-p)
		      (buffer-substring-no-properties
		       (region-beginning)
		       (region-end))
	        (let ((sym (thing-at-point 'symbol)))
		      (when (stringp sym)
		        (regexp-quote sym))))
	      regexp-history)
    (call-interactively 'occur)))

;; To refine your occur buffer, removing or keeping lines that match any regular expression of your choice, press "k" ([K]eep) or "f" ([F]lush). Press "u" to undo the last filter in the stack.
;; "k" occur-x-filter-out
;; "f" occur-x-filter
;; "u" occur-x-undo-filter
(use-package occur-x
  :disabled t
  :hook
  (occur-mode . turn-on-occur-x-mode))

(use-package occur-context-resize
  :disabled t
  :hook
  (occur-mode . occur-context-resize-mode))

(use-package counsel-ag-popup
  :disabled t)

;; fuzzy file finder
(use-package fiplr
  :disabled t
  :bind
  ("C-c p p" . fiplr-find-file)
  :config
  (setq fiplr-root-markers '(".git" ".svn"))
  (setq fiplr-ignored-globs '((directories (".git" ".svn" "node_modules" "elpa" "localelpa"))
                              (files ("*.jpg" "*.png" "*.zip" "*~")))))

;; https://github.com/winterTTr/ace-jump-mode
(use-package ace-jump-mode
  :disabled t
  :demand t
  :bind
  (("C-x j" . ace-jump-mode)
   ("M-g f" . ace-jump-mode))
  :config
  ;; Thanks to: https://github.com/winterTTr/ace-jump-mode/issues/23
  (defun ace-jump-two-chars-mode (&optional query-char query-char-2)
    "AceJump two chars mode"
    (interactive)

    (setq query-char (or query-char (read-char ">")))
    (setq query-char-2 (or query-char-2 (read-char (concat ">" (string query-char)))))

    (if (eq (ace-jump-char-category query-char) 'other)
        (error "[AceJump] Non-printable character"))

    ;; others : digit , alpha, punc
    (setq ace-jump-query-char query-char)
    (setq ace-jump-current-mode 'ace-jump-char-mode)
    (ace-jump-do (regexp-quote (concat (char-to-string query-char)
                                       (char-to-string query-char-2))))))

;; Goto last change
(use-package goto-chg
  :disabled t
  :bind
  (("C-c b ," . goto-last-change)
   ("C-c b ." . goto-last-change-reverse)))

(use-package fuz
  :disabled t
  :load-path "localelpa/fuz.el"
  :config
  (unless (require 'fuz-core nil t)
    (fuz-build-and-load-dymod)))
;;================================================================================

(provide 'init-search)
;;; init-search.el ends here
