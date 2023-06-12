;; init-blog.el --- Initialize blog settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code

;; hexo 博客系统
(require 'ox-publish)
(defun org-custom-link-img-follow (path)
  (org-open-file-with-emacs
   (format "../source/assets/%s" path)))   ;the path of the image in local dic

(defun org-custom-link-img-export (path desc format)
  (cond
    ((eq format 'html)
     (format "<img src=\"/assets/%s\" alt=\"%s\"/>" path desc)))) ;the path of the image in webserver

(org-add-link-type "img" 'org-custom-link-img-follow 'org-custom-link-img-export)

(use-package easy-hugo
  :init
  (setq easy-hugo-basedir "~/src/yangc/itsyc.xyz/"
        easy-hugo-url "http://itsyc.xyz"
        easy-hugo-root "~/src/yangc/itsyc.xyz/"
        easy-hugo-previewtime "300")
  :config
  (setq easy-hugo-default-ext ".org")
  ;; (when (featurep 'evil)
  ;;   (setq evil-emacs-state-modes (delq 'easy-hugo-mode evil-emacs-state-modes)))
  :general ("C-c C-e" 'easy-hugo))

;; - https://github.com/kaushalmodi/ox-hugo
;; - https://ox-hugo.scripter.co/doc/custom-front-matter/
(use-package ox-hugo
  :after ox)

(provide 'init-blog)
