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

(provide 'init-dashboard)
