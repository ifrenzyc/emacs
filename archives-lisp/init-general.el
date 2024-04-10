;; init-general.el --- Initialize general settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; - leader key 主要参考了这个设置：https://github.com/yanghaoxie/emacs-dotfile#install-general
;; - https://sam217pa.github.io/2016/09/23/keybindings-strategies-in-emacs/
;; 

;;; Code:
(require 'init-funcs)

(use-package general
  :disabled t
  :demand t
  ;; - https://github.com/noctuid/general.el
  ;; 参考这篇文章重新定义自己的 key bindings：https://leiyue.wordpress.com/2012/07/04/use-org-mode-and-taskjuggler-to-manage-to-project-information/
  ;; 参考在 Mac 下的一些配置：https://www.emacswiki.org/emacs/EmacsForMacOS
  )

(provide 'init-general)
;;; init-general.el ends here
