;;; init.el --- Emacs Initialization File

;;; Commentary:

;; Copyright (c) 2016-2018 Yang Chuang
;;
;; Author: Yang Chuang <ifrenzyc@gmail.com>
;; URL: https://github.com/ifrenzyc
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Code:

;; Added by package.el. This must come before configurations of
;; installed packages. Don't delete this line. If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; uncomment this line in first time startup.
;; (package-initialize)

(require 'org)
(require 'ob-tangle)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (org . t)
   (shell . t)))

(org-babel-load-file (expand-file-name "~/.emacs.d/emacs.org"))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;;; init.el ends here
