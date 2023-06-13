;; init-const.el --- Initialize constant variables configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:
(defconst yc/cache-dir (expand-file-name ".cache" user-emacs-directory)
  "Every cached or moving file should be here like with Spacemacs")
(make-directory yc/cache-dir t)

;; Detect Operating System
(defconst IS-MAC   (eq system-type 'darwin))
(defconst IS-LINUX (eq system-type 'gnu/linux))
(defconst IS-BSD   (eq system-type 'gnu/kfreebsd))
(defconst IS-WINDOWS (or (eq system-type 'ms-dos)
                         (eq system-type 'windows-nt)
                         (eq system-type 'cygwin)))
(provide 'init-const)
;;; init-const.el ends here
