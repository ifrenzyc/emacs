;; init-cnfonts.el --- Initialize cnfonts configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; tumashu/cnfonts: emacs 中文字体配置工具。可以快速方便的的实现中文字体和英文字体等宽（也就是常说的中英文对齐）。
;; - https://github.com/tumashu/cnfonts
;;
;; `cnfonts--custom-set-fontsnames' 列表有 3 个子列表，第 1 个为英文字体列表，第 2 个为中文字体列表，
;; 第 3 个列表中的字体用于显示不常用汉字，每一个字体列表中，*第一个* *有效并可用* 的字体将被使用。
;; 将光标移动到上述列表中，按 `C-c C-c' 可以测试字体显示效果。另外，用户可以通过命令
;; `cnfonts-insert-fontname’ 来选择一个 *可用* 字体，然后在当前光标处插入其字体名称。
;; (setq cnfonts--custom-set-fontnames
;;       '(
;;         ("Source Code Pro")
;;         ("Hiragino Sans GB")
;;         ("HanaMinB")
;;         ))
;;
;; `cnfonts--custom-set-fontsizes' 中，所有元素的结构都类似：(英文字号 中文字号 EXT-B 字体字号)
;; 将光标移动到各个数字上，按 C-c C-c 查看光标处字号的对齐效果。
;; 按 C-<up> 增大光标处字号，按 C-<down> 减小光标处字号。
;; (setq cnfonts--custom-set-fontsizes
;;       '(
;;         (9    10.0 10.0)
;;         (10   12.0 12.0)
;;         (11.5 15.0 15.0)
;;         (12.5 15.0 15.0)
;;         (14   16.0 16.0)
;;         (16   20.0 20.0)
;;         (18   22.0 22.0)
;;         (20   24.0 24.0)
;;         (22   26.0 26.0)
;;         (24   28.0 28.0)
;;         (26   32.0 32.0)
;;         (28   34.0 34.0)
;;         (30   36.0 36.0)
;;         (32   38.0 38.0)
;;         ))
;; 

;;; Code:

(use-package cnfonts
  ;; :hook
  ;; (cnfonts-set-font-finish . yc/symbol-fonts)
  :init
  (setq cnfonts-verbose nil)
  (setq cnfonts-profiles
        '("org-mode" "program" "read-book"))
  :bind
  (([remap text-scale-increase] . cnfonts-increase-fontsize)
   ([remap text-scale-decrease] . cnfonts-decrease-fontsize)
   ([remap text-scale-adjust] . cnfonts-reset-fontsize))
  :config
  (cnfonts-enable)
  ;; (setq cnfonts-use-face-font-rescale t)    ; 用于设置不同标题中文字体大小不同 , 比如 emacs 自带的 lenven 主题就支持这一特性.
  ;; (defun yc/symbol-fonts (fontsizes-list)
  ;;   (let* ((fontname "Quivira")
  ;;          (fontsize (nth 0 fontsizes-list))
  ;;          (fontspec (font-spec :name fontname
  ;;                               :size 14
  ;;                               :weight 'normal
  ;;                               :slant 'normal)))
  ;;     (if (cnfonts--fontspec-valid-p fontspec)
  ;;         (set-fontset-font "fontset-default" 'symbol fontspec nil 'append)
  ;;       (message "字体 %S 不存在！" fontname))))
  )

(provide 'init-cnfonts)
