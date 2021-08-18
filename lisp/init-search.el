;; init-search.el --- Initialize search configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:

(use-package isearch
  :straight (:type built-in)
  :no-require t
  :commands swiper-from-isearch
  :bind (("M-s ." . isearch-forward-symbol-at-point)
         ;; ("C-s" . isearch-forward-regexp)
         ("C-r" . isearch-backward-regexp)
         :map isearch-mode-map
         ("C-;" . swiper-from-isearch)
         ("C-'" . avy-isearch)
         ("C-l" . counsel-git-grep-from-isearch)))

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
    (isearch-yank-string (buffer-substring-no-properties $p1 $p2))))

(general-define-key "M-s SPC" 'xah-search-current-word)

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
  (call-interactively 'occur))

(global-set-key (kbd "M-s o") 'occur-dwim)

;; 快速在当前 buffer 中跳转光标
;; - https://github.com/abo-abo/avy
;; - http://ergoemacs.org/misc/ace_jump_avy_vs_isearch.html
(use-package avy
  :custom ((avy-all-windows nil)
           (avy-all-windows-alt t)
           (avy-background t)
           (avy-style 'pre)
           (avy-keys '(?h ?t ?n ?s ?m ?w ?v ?z)))
  :hook (after-init . avy-setup-default)
  :general
  ("C-;"   'ace-jump-two-chars-mode
   "C-:"   'avy-goto-char-2
   "M-g A" 'avy-goto-char-timer
   "M-g a" 'avy-goto-char
   "M-g l" 'avy-goto-line)
  :config
  (defhydra hydra-avy (:color red)
    "avy-goto"
    ("c" avy-goto-char "char")
    ("C" avy-goto-char-2 "char-2")
    ("w" avy-goto-word-1 "word")
    ("l" avy-goto-line "line")
    ("s" avy-goto-subword-1 "subword")
    ("u" link-hint-open-link "open-URI")
    ("U" link-hint-copy-link "copy-URI")))

;; https://github.com/winterTTr/ace-jump-mode
(use-package ace-jump-mode
  :demand t
  :bind
  (("C-x j" . ace-jump-mode)
   ("M-g f" . ace-jump-mode))
  :config
  ;; Thanks to: https://github.com/winterTTr/ace-jump-mode/issues/23
  (defun ace-jump-two-chars-mode (&optional query-char query-char-2)
    "AceJump two chars mode"
    (interactive)

    (evil-half-cursor)
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
  :config
  (global-set-key (kbd "C-c b ,") 'goto-last-change)
  (global-set-key (kbd "C-c b .") 'goto-last-change-reverse))

(use-package fzf)

;; fuzzy file finder
(use-package fiplr
  :general
  ("C-c p p" 'fiplr-find-file)
  :config
  (setq fiplr-root-markers '(".git" ".svn"))
  (setq fiplr-ignored-globs '((directories (".git" ".svn" "node_modules" "elpa"))
                              (files ("*.jpg" "*.png" "*.zip" "*~")))))

;; 基于 Silver Searcher 的多文件内容查找
;; - https://github.com/Wilfred/ag.el
(use-package ag
  :ensure-system-package (ag . "brew install the_silver_searcher"))

(use-package rg
  :config
  ;; (evil-set-initial-state 'rg-mode 'emacs)
  (setq rg-group-result t)
  (setq rg-show-header t))

;; https://github.com/manateelazycat/color-rg
(use-package color-rg
  :ensure-system-package (rg . "brew install ripgrep")
  :straight (:host github
                   :repo "manateelazycat/color-rg"
                   :files ("color-rg.el"))
  ;; :commands (color-rg-search-input
  ;;            color-rg-search-symbol
  ;;            color-rg-search-project)
  :bind
  (("M-s M-s" . color-rg-search-project)))

;; *About:* 多文件查找替换
;; - https://github.com/mhayashi1120/Emacs-wgrep
;; - https://www.reddit.com/r/emacs/comments/ce0557/emacs_editing_multiple_files_with_grep_wgrep/
(use-package wgrep
  :commands (wgrep-change-to-wgrep-mode ivy-wgrep-change-to-wgrep-mode)
  :config
  (setq wgrep-enable-key "r"
        wgrep-auto-save-buffer t))

(use-package ripgrep :commands ripgrep-regexp)

(use-package deadgrep)

(provide 'init-search)
