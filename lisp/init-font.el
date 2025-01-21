;; init-font.el --- Initialize font settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; - https://github.com/shimmy1996/.emacs.d#fontset-with-cjk-and-unicode-fallback
;; - http://ergoemacs.org/emacs/emacs_list_and_set_font.html
;; - https://archive.casouri.co.uk/note/2019/emacs-%E5%AD%97%E4%BD%93%E4%B8%8E%E5%AD%97%E4%BD%93%E9%9B%86/index.html
;; 

;;; Code:

;; JetBrains Mono
;; Roboto Mono
;; IBM Plex Mono
;; IBM Plex Sans SC
;; Hack
;; LXGW WenKai Mono GB
;; 中文的字体
(defvar user/cjk-font "LXGW WenKai Mono GB"
  "Default font for CJK characters.")

(defvar user/latin-font "JetBrains Mono"
  "Default font for Latin characters.")

(defvar user/unicode-font "Hack"
  "Default font for Unicode characters, including emojis.")

(defvar user/font-size 13
  "Default font size in px.")

(defvar user/standard-fontset
  (create-fontset-from-fontset-spec standard-fontset-spec)
  "Standard fontset for user.")

;; Ensure user/standard-fontset gets used for new frames.
(add-to-list 'default-frame-alist (cons 'font user/standard-fontset))
(add-to-list 'initial-frame-alist (cons 'font user/standard-fontset))

;; Enable font customization for charset 'symbols, which contains puncuation
;; marks, emoji, etc.
(setq use-default-font-for-symbols nil)

(defun user/set-font ()
  "Set Unicode, Latin and CJK font for user/standard-fontset."
  ;; Unicode font.
  (set-fontset-font user/standard-fontset 'unicode
                    (font-spec :family user/unicode-font)
                    nil 'prepend)
  ;; Latin font.
  ;; Only specify size here to allow text-scale-adjust work on other fonts.
  (set-fontset-font user/standard-fontset 'latin
                    (font-spec :family user/latin-font :size user/font-size)
                    nil 'prepend)
  ;; CJK font.
  (dolist (charset '(kana han cjk-misc hangul kanbun bopomofo))
    (set-fontset-font user/standard-fontset charset
                      (font-spec :family user/cjk-font)
                      nil 'prepend))
  ;; Special settings for certain CJK puncuation marks.
  ;; These are full-width characters but by default uses half-width glyphs.
  (dolist (charset '((#x2018 . #x2019)    ;; Curly single quotes "‘’"
                     (#xFF08 . #xFF09)    ;; Curly "（）"
                     (#x201c . #x201d)))  ;; Curly double quotes "“”"
    (set-fontset-font user/standard-fontset charset
                      (font-spec :family user/cjk-font)
                      nil 'prepend)))

;; Apply changes.
(user/set-font)
;; For emacsclient.
(add-hook 'before-make-frame-hook #'user/set-font)
;; (setq-default line-spacing 0.2)

;;================================================================================
(use-package fixed-pitch
  :disabled t
  :load-path "localelpa/fixed-pitch-mode"
  :custom
  (fixed-pitch-dont-change-cursor t)
  (fixed-pitch-use-extended-default t)
  :init
  (require 'fixed-pitch))

;; Unicode is an required aesthetic
;; (use-package pcache ;; Required by unicode-fonts
;;   :init
;;   ;; Mentioned here to redirect directory
;;   (setq pcache-directory (expand-file-name "pcache/" yc/cache-dir)))

;; (use-package unicode-fonts
;;   :config
;;   (unicode-fonts-setup))
;;================================================================================

(provide 'init-font)
;;; init-font.el ends here
