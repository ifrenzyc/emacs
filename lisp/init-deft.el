;; init-deft.el --- Initialize deft settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; - https://jblevins.org/projects/deft/
;; - https://www.reddit.com/r/emacs/comments/agw3o5/3500_note_files_40_mb_of_plain_text_100s_of_tags/
;; - https://github.com/EFLS/zetteldeft
;; 

;;; Code
(use-package deft
  :commands (deft deft-open-file deft-new-file-named)
  ;; :general
  ;; (yc/leader-keys
  ;;   "d" '(:ignore t :which-key "deft")
  ;;   "dd" '(deft :which-key "deft"))
  :config
  (setq deft-directory "~/notes/Zettelkästen"
        deft-recursive t
        deft-text-mode 'org-mode
        deft-default-extension "org"
        deft-extensions '("md" "txt" "org" "tex")
        deft-use-filter-string-for-filename t
        deft-use-filename-as-title nil
        deft-org-mode-title-prefix t
        deft-markdown-mode-title-level 1
        deft-file-naming-rules '((noslash . "-")
                                 (nospace . "-")
                                 (case-fn . downcase))
        deft-strip-summary-regexp (concat "\\("
                                          "[\n\t]" ;; blank
                                          "\\|^#\\+[[:upper:]_]+:.*$" ;; org-mode metadata
                                          "\\|^#\\+[[:alnum:]_]+:.*$" ;; org-mode metadata
                                          "\\)")))

(use-package zetteldeft
  :after deft
  ;; :general
  ;; (yc/leader-keys
  ;;   "dD" '(zetteldeft-deft-new-search :which-key "new search")
  ;;   "dR" '(deft-refresh :which-key "refresh")
  ;;   "ds" '(zetteldeft-search-at-point :which-key "search at point")
  ;;   "dc" '(zetteldeft-search-current-id :which-key "search current id")
  ;;   "df" '(zetteldeft-follow-link :which-key "follow link")
  ;;   "dF" '(zetteldeft-avy-file-search-ace-window :which-key "avy file other window")
  ;;   "dl" '(zetteldeft-avy-link-search :which-key "avy link search")
  ;;   "dt" '(zetteldeft-avy-tag-search :which-key "avy tag search")
  ;;   "dT" '(zetteldeft-tag-buffer :which-key "tag list")
  ;;   "di" '(zetteldeft-find-file-id-insert :which-key "insert id")
  ;;   "dI" '(zetteldeft-find-file-full-title-insert :which-key "insert full title")
  ;;   "do" '(zetteldeft-find-file :which-key "find file")
  ;;   "dn" '(zetteldeft-new-file :which-key "new file")
  ;;   "dN" '(zetteldeft-new-file-and-link :which-key "new file & link")
  ;;   "dr" '(zetteldeft-file-rename :which-key "rename")
  ;;   "dx" '(zetteldeft-count-words :which-key "count words"))
  )

(provide 'init-deft)
