;; init-dashboard.el --- Initialize dashboard configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 修改 Emacs 的启动界面，展现 Dashboard，特别是 Recent files（最近打开的列表），特别适用于我这种基本采用打开最近文件的方式。
;; Dashboard 的加载需要在 theme 和 modeline 前面，不然 modeline 加载不了。
;; - https://github.com/emacs-dashboard/emacs-dashboard
;; 

;;; Code:

;; No Dashbaord
(setq inhibit-startup-screen t)

;; (use-package dashboard
;;   :config
;;   ;; Set the banner
;;   (setq dashboard-startup-banner 2)
;;   ;; Value can be
;;   ;; 'official which displays the official emacs logo
;;   ;; 'logo which displays an alternative emacs logo
;;   ;; 1, 2 or 3 which displays one of the text banners
;;   ;; "path/to/your/image.png" which displays whatever image you would prefer

;;   (setq dashboard-items '((recents  . 5)
;;                           (projects . 5)
;;                           (bookmarks . 5)))
;;   ;; Set the title
;;   (setq dashboard-banner-logo-title "[ E M A C S ]")
;;   ;; (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
;;   ;; Content is not centered by default. To center, set
;;   (setq dashboard-center-content t)

;;   ;; To disable shortcut "jump" indicators for each section, set
;;   (setq dashboard-show-shortcuts nil)
;;   (dashboard-setup-startup-hook)
;;   (add-to-list 'dashboard-items '(agenda) t)
;;   (setq show-week-agenda-p t))

;; Save and restore Emacs status, including buffers, point and window configurations.
;; - https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org#dotemacs
;; (use-package desktop
;;   :config
;;   (desktop-save-mode 1)
;;   (add-to-list 'desktop-globals-to-save 'golden-ratio-adjust-factor)
;;   (push '(company-posframe-mode . nil)
;;         desktop-minor-mode-table)
;;   (defhydra hydra-desktop (:color blue)
;;     "Desktop"
;;     ("c" desktop-clear "clear")
;;     ("s" desktop-save "save")
;;     ("r" desktop-revert "revert")
;;     ("d" desktop-change-dir "dir")))

(provide 'init-dashboard)
