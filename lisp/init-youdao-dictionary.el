;; init-youdao-dictionary.el --- Initialize youdao-dictionary settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:
(use-package youdao-dictionary
  :functions (posframe-show posframe-hide)
  ;; :bind
  ;; (("C-c y y" . youdao-dictionary-search-at-point+)
  ;;  ("C-c y i" . youdao-dictionary-search-at-point)
  ;;  ("C-c Y"   . yc/youdao-search-at-point))
  :preface
  (with-eval-after-load 'posframe
    (defun youdao-dictionary-search-at-point-posframe ()
      "Search word at point and display result with `posframe'."
      (interactive)
      (let ((word (youdao-dictionary--region-or-word)))
        (if word
            (progn
              (with-current-buffer (get-buffer-create youdao-dictionary-buffer-name)
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (youdao-dictionary-mode)
                  (insert (youdao-dictionary--format-result word))
                  (goto-char (point-min))
                  (set (make-local-variable 'youdao-dictionary-current-buffer-word) word)))
              (posframe-show youdao-dictionary-buffer-name
                             :position (point))
              (unwind-protect
                  (push (read-event) unread-command-events)
                (posframe-hide youdao-dictionary-buffer-name)))
          (message "Nothing to look up")))))
  :config
  (defun yc/youdao-search-at-point ()
    (interactive)
    (if (display-graphic-p)
        (if (fboundp 'youdao-dictionary-search-at-point-posframe)
            (youdao-dictionary-search-at-point-posframe)
          (youdao-dictionary-search-at-point-tooltip))))
  :custom
  ;; Cache documents
  (url-automatic-caching t)

  ;; Enable Chinese word segmentation support (支持中文分词)
  (youdao-dictionary-use-chinese-word-segmentation t))

(use-package fanyi
  :commands fanyi-dwim fanyi-dwim2
  :custom (fanyi-providers '(fanyi-haici-provider
                             fanyi-youdao-thesaurus-provider
                             fanyi-etymon-provider
                             fanyi-longman-provider)))

;; @see - https://github.com/lorniu/go-translate
(use-package go-translate
  :commands (gt-do-translate)
  ;; :bind
  ;; ("M-E" . gt-do-translate)
  :init
  (setq gt-langs '("en" "zh")
        gt-buffer-render-follow-p t
        gt-buffer-render-window-config
        '((display-buffer-reuse-window display-buffer-in-direction)
          (direction . bottom)
          (window-height . 0.4)))

  (setq gt-pop-posframe-forecolor (face-foreground 'tooltip nil t)
        gt-pop-posframe-backcolor (face-background 'tooltip nil t))
  (when (facep 'posframe-border)
    (setq gt-pin-posframe-bdcolor (face-background 'posframe-border nil t))))

(provide 'init-youdao-dictionary)
;;; init-youdao-dictionary.el ends here
