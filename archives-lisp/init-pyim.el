;; init-pyim.el --- Initialize Chinese input method configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; (use-package liberime-config
;;   :straight (
;;            :host git
;;            :repo "https://gitlab.com/liberime/liberime"
;;            :files ("CMakeLists.txt" "Makefile" "src" "liberime-config.el"))
;;   :init
;;   (add-hook 'after-liberime-load-hook
;;             (lambda ()
;;               (liberime-select-schema "luna_pinyin_simp"))))

(use-package liberime-config
  ;; :straight (:host github
  ;;                  :repo "merrickluo/liberime"
  ;;                  :files ("CMakeLists.txt" "*.el" "README.org" "Makefile" "src"))
  :init
  (add-hook 'liberime-after-start-hook
            (lambda ()
              (liberime-select-schema "luna_pinyin_simp")
              (liberime-start "/Library/Input Methods/Squirrel.app/Contents/SharedSupport" (file-truename (expand-file-name "rime/" user-emacs-directory)))
              ))
  :config
  (require 'liberime)  ; use RIME
  (liberime-load))

(use-package pyim
  ;; :demand t
  :after (ivy liberime liberime-config)
  ;; :init
  :config
  (setq default-input-method "pyim")
  (setq pyim-title "ㄓ")

  ;; (liberime-start "/Library/Input Methods/Squirrel.app/Contents/SharedSupport" (file-truename (expand-file-name "rime/" user-emacs-directory)))
  ;; (liberime-select-schema "luna_pinyin_simp")
  (setq pyim-default-scheme 'rime-quanpin)  ;; 使用全拼

  ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
  ;; 我自己使用的中英文动态切换规则是：
  ;; 1. 光标只有在注释里面时，才可以输入中文。
  ;; 2. 光标前是汉字字符时，才能输入中文。
  ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  ;; pyim-probe-evil-normal-mode
                  pyim-probe-org-structure-template))

  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))

  ;; 开启拼音搜索功能
  (pyim-isearch-mode 1)

  ;; 使用 popup-el 来绘制选词框, 如果用 emacs26, 建议设置
  ;; 为 'posframe, 速度很快并且菜单不会变形，不过需要用户
  ;; 手动安装 posframe 包。
  (setq pyim-page-tooltip 'popup)
  ;; (setq pyim-page-tooltip 'posframe)
  ;; (setq pyim-page-tooltip 'child-frame)

  ;; 选词框显示 5 个候选词
  (setq pyim-page-length 8)

  ;; ;; @see - [[https://emacs-china.org/t/topic/6069/8][怎样给 Ivy 添加拼音支持 - Emacs-general - Emacs China]]
  ;; (defun eh-ivy-cregexp (str)
  ;;   (concat
  ;;    (ivy--regex-plus str)
  ;;    "\\|"
  ;;    (pyim-cregexp-build str)))

  ;; (setq ivy-re-builders-alist
  ;;       '((t . eh-ivy-cregexp)))
  :bind
  (("M-j" . pyim-convert-string-at-point)  ; 与 pyim-probe-dynamic-english 配合
   ("s-\\" . pyim-convert-string-at-point)
   ;; ("C-;" . pyim-delete-word-from-personal-buffer)
   ))

;; 激活 basedict 拼音词库，五笔用户请继续阅读 README
(use-package pyim-basedict
  :after pyim
  :init
  (setq pyim-dcache-auto-update nil)
  :config (pyim-basedict-enable))

(provide 'init-pyim)
