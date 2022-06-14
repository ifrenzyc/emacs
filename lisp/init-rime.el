;; init-rime.el --- Initialize Chinese input method configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Emacs 下的中文输入法，支持全拼，双拼，五笔和仓颉，在线文档：http://tumashu.github.io/pyim
;; - https://manateelazycat.github.io/emacs/2019/07/24/use-rime-in-emacs.html
;; - https://emacs-china.org/t/mac-emacs-rime/10063/11
;; - https://gitlab.com/liberime/liberime
;; - https://github.com/QiangF/liberime
;; - https://github.com/merrickluo/liberime
;; 
;; - https://github.com/DogLooksGood/emacs-rime
;; - https://emacs-china.org/t/topic/17717 

;;; Code:

(use-package rime
  ;; :straight (:host github
  ;;                  :repo "DogLooksGood/emacs-rime"
  ;;                  :files ("*.el" "Makefile" "lib.c"))
  :custom
  (rime-librime-root (expand-file-name "librime/dist" user-emacs-directory))
  (rime-user-data-dir (expand-file-name "rime" user-emacs-directory))
  (default-input-method "rime")
  (rime-title "ㄓ")
  (rime-show-candidate 'posframe)
  (rime-posframe-properties
   (list :background-color "#333333"
         :foreground-color "#dcdccc"
         :font "LXGW WenKai Mono-14"
         :internal-border-width 1))
  :general
  (yc/nonprefix-keys
      :keymaps 'rime-mode-map
    "C-`" 'rime-send-keybinding
    "M-j" 'rime-force-enable)
  :config
  ;; 增加断言列表，当其中有任何一个断言的值不是 nil 时，会自动使用英文。
  (setq rime-disable-predicates
        '(;; rime-predicate-evil-mode-p  ;; evil-normal-state
          rime-predicate-after-ascii-char-p     ;; 任意英文字符后
          rime-predicate-after-alphabet-char-p  ;; 在英文字符串之后（必须为以字母开头的英文字符串）
          rime-predicate-ace-window-p           ;; 激活 ace-window-mode
          rime-predicate-hydra-p                ;; 激活了一个 hydra keymap
          rime-predicate-punctuation-after-space-cc-p  ;; 当要在中文字符且有空格之后输入符号时
          rime-predicate-space-after-cc-p       ;; 在中文字符且有空格之后
          rime-predicate-prog-in-code-p))  ;; 代码中自动使用英文
  (setq rime-cursor "˰")
  ;; 默认值
  (setq rime-translate-keybindings
        '("C-f" "C-b" "C-n" "C-p" "C-g")))

;; change cursor color
(add-to-list 'load-path (expand-file-name "localelpa/im-cursor-chg" user-emacs-directory))
(with-eval-after-load 'rime
  (require 'im-cursor-chg)
  (cursor-chg-mode 1))


;; 在 Emacs 中使用原生的输入法
;; - https://emacs-china.org/t/smart-input-source-evil/12592/37
;; (use-package sis
;;   :init
;;   ;; `C-s/r' 默认优先使用英文 必须在 sis-global-respect-mode 前配置
;;   (setq sis-respect-go-english-triggers
;;         (list 'isearch-forward 'isearch-backward) ; isearch-forward 命令时默认进入en
;;         sis-respect-restore-triggers
;;         (list 'isearch-exit 'isearch-abort)) ; isearch-forward 恢复, isearch-exit `<Enter>', isearch-abor `C-g'
;;   :config
;;   ;; (sis-ism-lazyman-config "1033" "2052" 'im-select) ; 输入码 1033/英文，2052/中文小狼毫
;;   (sis-ism-lazyman-config
;;    "com.apple.keylayout.US"
;;    "im.rime.inputmethod.Squirrel.Rime")
;;   ;; enable the /cursor color/ mode 中英文光标颜色模式
;;   (sis-global-cursor-color-mode t)
;;   ;; enable the /respect/ mode buffer 输入法状态记忆模式
;;   (sis-global-respect-mode t)
;;   ;; enable the /follow context/ mode for all buffers
;;   (sis-global-follow-context-mode t)
;;   ;; enable the /inline english/ mode for all buffers
;;   (sis-global-inline-mode t) ; 中文输入法状态下，中文后<spc>自动切换英文，结束后自动切回中文
;;   ;; (global-set-key (kbd "<f9>") 'sis-log-mode) ; 开启日志
;;   ;; 特殊定制
;;   (setq sis-default-cursor-color "green yellow" ; 英文光标色
;;         sis-other-cursor-color "#FF2121" ; 中文光标色
;;         ;; sis-inline-tighten-head-rule 'all ; 删除头部空格，默认1，删除一个空格，1/0/'all
;;         sis-inline-tighten-tail-rule 'all ; 删除尾部空格，默认1，删除一个空格，1/0/'all
;;         sis-inline-with-english t ; 默认是t, 中文context下输入<spc>进入内联英文
;;         sis-inline-with-other t) ; 默认是nil，而且prog-mode不建议开启, 英文context下输入<spc><spc>进行内联中文
;;   ;; 特殊buffer禁用sis前缀,使用Emacs原生快捷键  setqsis-prefix-override-buffer-disable-predicates
;;   (setq sis-prefix-override-buffer-disable-predicates
;;         (list 'minibufferp
;;               (lambda (buffer) ; magit revision magit的keymap是基于text property的，优先级比sis更高。进入 magit 后，disable sis的映射
;;                 (sis--string-match-p "^magit-revision:" (buffer-name buffer)))
;;               (lambda (buffer) ; special buffer，所有*打头的buffer，但是不包括*Scratch* *New, *About GNU等buffer
;;                 (and (sis--string-match-p "^\*" (buffer-name buffer))
;;                      (not (sis--string-match-p "^\*About GNU Emacs" (buffer-name buffer))) ; *About GNU Emacs" 仍可使用 C-h/C-x/C-c 前缀
;;                      (not (sis--string-match-p "^\*New" (buffer-name buffer)))
;;                      (not (sis--string-match-p "^\*Scratch" (buffer-name buffer))))))) ; *Scratch*  仍可使用 C-h/C-x/C-c 前缀
;;   )

(provide 'init-rime)
