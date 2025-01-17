;; init-gc.el --- Initialize gc configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:
;; 列出 Emacs 触发的函数或按键历史操作列表
;; - https://github.com/lewang/command-log-mode
;; - https://github.com/michael-heerdegen/interaction-log.el
;; 
;; | keybindings | function                      | 描述    |
;; |-------------+-------------------------------+---------|
;; | ~C-c o~     | clm/toggle-command-log-buffer | 默认按键 |
(use-package command-log-mode
  :commands (command-log-mode))

(use-package manage-minor-mode
  :commands manage-minor-mode)

;; 参考 doom-emacs 调整 GC：https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast
;; - http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
;; Then reset it as late as possible; these are the reasonable defaults I use.
(add-hook 'emacs-startup-hook #'(lambda () (setq gc-cons-threshold 16777216
                                                 gc-cons-percentage 0.1)))
(run-with-idle-timer 3 t (lambda () (garbage-collect)))

;;================================================================================
;; (use-package benchmark-init
;;   :init
;;   (benchmark-init/activate)
;;   :hook
;;   (after-init . benchmark-init/deactivate))

(use-package esup
  :disabled t)

;; Hunt down errors by bisecting elisp files
;; (use-package bug-hunter)

;;; GC optimization
;; Use GCMH --  the Garbage Collector Magic Hack -- to adjust garbage collection.
;; - https://gitlab.com/koral/gcmh
;; - https://emacs-china.org/t/emacs-10/17716
;; (use-package gcmh
;;   :demand t
;;   :init
;;   (add-hook 'after-init-hook 'gcmh-mode)
;;   ;; (setq gcmh-verbose             t
;;   ;;       gcmh-lows-cons-threshold #x800000
;;   ;;       gcmh-high-cons-threshold most-positive-fixnum
;;   ;;       gcmh-idle-delay          3600)
;;   :custom
;;   (gcmh-idle-delay 10)
;;   (gcmh-high-cons-threshold #x6400000))
;;================================================================================

(provide 'init-gc)
;;; init-gc.el ends here
