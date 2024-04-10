;; init-general.el --- Initialize general settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; - leader key 主要参考了这个设置：https://github.com/yanghaoxie/emacs-dotfile#install-general
;; - https://sam217pa.github.io/2016/09/23/keybindings-strategies-in-emacs/
;; 

;;; Code:
(require 'init-funcs)

(use-package general
  :demand t
  ;; - https://github.com/noctuid/general.el
  ;; 参考这篇文章重新定义自己的 key bindings：https://leiyue.wordpress.com/2012/07/04/use-org-mode-and-taskjuggler-to-manage-to-project-information/
  ;; 参考在 Mac 下的一些配置：https://www.emacswiki.org/emacs/EmacsForMacOS
  )


(global-set-key (kbd "C-x 2") 'yc/split-window-vertically)
(global-set-key (kbd "C-x 3") 'yc/split-window-horizontally)

(general-define-key "C-c n n" 'yc/new-buffer-frame)
(general-define-key "C-x C-m" 'yc/move-file)

(provide 'init-general)
;;; init-general.el ends here
