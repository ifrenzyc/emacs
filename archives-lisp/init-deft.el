;; init-deft.el --- Initialize deft settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; - https://jblevins.org/projects/deft/
;; - https://www.reddit.com/r/emacs/comments/agw3o5/3500_note_files_40_mb_of_plain_text_100s_of_tags/
;; - https://github.com/EFLS/zetteldeft
;; 

;;; Code:
(use-package deft
  :commands (deft deft-open-file deft-new-file-named)
  ;; :general
  ;; (yc/leader-keys
  ;;   "d" '(:ignore t :wk "deft")
  ;;   "dd" '(deft :wk "deft"))
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
  ;;   "dD" '(zetteldeft-deft-new-search :wk "new search")
  ;;   "dR" '(deft-refresh :wk "refresh")
  ;;   "ds" '(zetteldeft-search-at-point :wk "search at point")
  ;;   "dc" '(zetteldeft-search-current-id :wk "search current id")
  ;;   "df" '(zetteldeft-follow-link :wk "follow link")
  ;;   "dF" '(zetteldeft-avy-file-search-ace-window :wk "avy file other window")
  ;;   "dl" '(zetteldeft-avy-link-search :wk "avy link search")
  ;;   "dt" '(zetteldeft-avy-tag-search :wk "avy tag search")
  ;;   "dT" '(zetteldeft-tag-buffer :wk "tag list")
  ;;   "di" '(zetteldeft-find-file-id-insert :wk "insert id")
  ;;   "dI" '(zetteldeft-find-file-full-title-insert :wk "insert full title")
  ;;   "do" '(zetteldeft-find-file :wk "find file")
  ;;   "dn" '(zetteldeft-new-file :wk "new file")
  ;;   "dN" '(zetteldeft-new-file-and-link :wk "new file & link")
  ;;   "dr" '(zetteldeft-file-rename :wk "rename")
  ;;   "dx" '(zetteldeft-count-words :wk "count words"))
  )

(provide 'init-deft)
;;; init-deft.el ends here
