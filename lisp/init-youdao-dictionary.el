;; init-youdao-dictionary.el --- Initialize youdao-dictionary settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:
(use-package youdao-dictionary
  :functions (posframe-show posframe-hide)
  :bind
  (("C-c y y" . youdao-dictionary-search-at-point+)
   ("C-c y i" . youdao-dictionary-search-at-point)
   ("C-c Y"   . my-youdao-search-at-point))
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
  (defun my-youdao-search-at-point ()
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

;; @see - https://github.com/lorniu/go-translate
(use-package go-translate
  :commands (gts-do-translate)
  :bind
  ("M-E" . gts-do-translate)
  :custom
  (gts-translate-list '(("en" "zh") ("zh" "en") ("jp" "zh")))
  (gts-default-translator
   (gts-translator
    :picker (gts-prompt-picker)
    :engines (list (gts-bing-engine) (gts-google-engine))
    :render (gts-buffer-render)
    ;; :render (gts-posframe-pop-render)
    )))

(provide 'init-youdao-dictionary)
;;; init-youdao-dictionary.el ends here
