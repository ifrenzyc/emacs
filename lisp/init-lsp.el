;; init-lsp.el --- Initialize lsp settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; - https://github.com/emacs-lsp/lsp-mode
;; 
;; - lsp-mode 的优化提速 :: http://blog.binchen.org/posts/how-to-speed-up-lsp-mode.html
;; 
;; 解决 macOS 下，使用 nvm 管理 nodejs，在执行 (lsp-install-server) 时报错 =env: node: No such file or directory=，原因应该是 lsp 在系统路径上找 node 和 npm 没找到
;; ln -s ~/.nvm/versions/node/v14.11.0/bin/npm /opt/homebrew/bin/npm
;; ln -s ~/.nvm/versions/node/v14.11.0/bin/node /opt/homebrew/bin/node
;; 

;;; Code:
(use-package lsp-mode
  :bind
  (:map lsp-command-map
        ("d" . lsp-find-definition)
        ("f" . lsp-find-references)
        ("H" . lsp-ui-doc-show)
        ("i" . lsp-goto-implementation)
        ("t" . lsp-goto-type-definition))
  (:map lsp-mode-map
        ("C-c C-d" . lsp-describe-thing-at-point))
  :commands (lsp lsp-deferred)
  :hook
  ((lsp      . lsp-lens-mode)
   (lsp-mode . lsp-enable-which-key-integration)
   ;; (lsp-after-open . lsp-enable-imenu)
   ;; (prog-mode . lsp-deferred)
   ;; (add-hook 'lsp-managed-mode-hook 'lsp-diagnostics-modeline-mode)
   ;; (lsp-managed-mode . (lambda ()
   ;;                       (with-eval-after-load 'company
   ;;                         (setq-local company-backends '((company-capf :with company-yasnippet)))
   ;;                         )))
   )
  :custom
  (lsp-auto-guess-root t)               ; Detect project root
  (lsp-prefer-flymake nil)              ; Use lsp-ui and flycheck
  (lsp-log-io nil)
  ;; 以下这些设置都是考虑性能问题，默认不开启
  ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
  (lsp-keep-workspace-alive nil)
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-text-document-color nil)
  (lsp-enable-indentation nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-signature-auto-activate nil)
  (lsp-signature-render-documentation nil)
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-modeline-workspace-status-enable nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-print-performance t)
  (lsp-idle-delay 0.500)
  (lsp-headerline-breadcrumb-enable-diagnostics nil)
  (flymake-fringe-indicator-position 'right-fringe)
  (lsp-signature-function 'lsp-signature-posframe)
  (lsp-eldoc-enable-hover nil)

  (lsp-completion-provider :none)   ;; we use Corfu!

  ;; go install github.com/sqls-server/sqls@latest
  (lsp-sqls-server "/opt/homebrew/opt/go/libexec/bin/sqls")

  (lsp-semantic-tokens-enable t)
  (lsp-enable-semantic-highlighting t)
  (lsp-progress-spinner-type 'horizontal-breathing)

  ;; @see https://emacs-lsp.github.io/lsp-mode/page/performance
  (read-process-output-max (* 1024 1024)) ;; 1mb
  ;; ;; Performance tweaks, see
  ;; ;; https://github.com/emacs-lsp/lsp-mode#performance
  ;; (setq gc-cons-threshold 100000000
  ;;       ;; lsp-file-watch-threshold 1000
  ;;       lsp-diagnostics-modeline-scope :project    ;; :project/:workspace/:file
  :init
  (advice-add 'lsp :before (lambda (&rest _args) (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht))))))

(use-package lsp-copilot
  :disabled t
  :load-path ("localelpa/lsp-copilot")
  :hook
  ((tsx-ts-mode
    js-ts-mode
    typescript-mode
    typescript-ts-mode
    java-mode
    java-ts-mode
    rjsx-mode
    less-css-mode
    web-mode
    python-ts-mode
    rust-mode
    rustic-mode
    rust-ts-mode
    toml-ts-mode
    conf-toml-mode
    bash-ts-mode) . lsp-copilot-mode)
  :custom
  (lsp-copilot-user-languages-config "/Users/yangc/.emacs.d/localelpa/lsp-copilot/languages.toml"))

(use-package lsp-ui
  :after lsp-mode
  :bind
  (:map lsp-mode-map
        ("C-c u" . lsp-ui-imenu))
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)   ; [M-.]
        ([remap xref-find-references]  . lsp-ui-peek-find-references)    ; [M-?]
        ("M-RET"  . lsp-ui-sideline-apply-code-actions))
  :custom-face
  (lsp-ui-sideline-code-action ((t (:inherit warning))))
  :hook
  (lsp-mode . lsp-ui-mode)
  (lsp-mode . lsp-ui-sideline-mode)
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-position 'top) ;; at-point
  (lsp-ui-doc-delay 0.2)     ;; 延迟 2s 再打开 doc 窗口
  ;; (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-doc-border "orange") ;; (face-foreground 'font-lock-comment-face nil t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-show-with-cursor nil) ;; disable cursor hover
  (lsp-ui-doc-show-with-mouse nil)  ;; disable mouse hover
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-lens-enable nil)
  (lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                         ,(face-foreground 'font-lock-string-face)
                         ,(face-foreground 'font-lock-constant-face)
                         ,(face-foreground 'font-lock-variable-name-face)))

  (lsp-ui-doc-use-webkit nil)    ; (featurep 'xwidget-internal)
  :config
  ;; https://github.com/emacs-lsp/lsp-ui/issues/441
  (defun lsp-ui-peek--peek-display (src1 src2)
    (-let* ((win-width (frame-width))
            (lsp-ui-peek-list-width (/ (frame-width) 2))
            (string (-some--> (-zip-fill "" src1 src2)
                      
                      (-map-indexed 'lsp-ui-peek--make-line it)
                      (-concat it (lsp-ui-peek--make-footer))))
            )
      (setq lsp-ui-peek--buffer (get-buffer-create " *lsp-peek--buffer*"))
      (posframe-show lsp-ui-peek--buffer
                     :string (mapconcat 'identity string "")
                     :min-width (frame-width)
                     :poshandler #'posframe-poshandler-frame-center)))

  (defun lsp-ui-peek--peek-destroy ()
    (when (bufferp lsp-ui-peek--buffer)
      (posframe-delete lsp-ui-peek--buffer))
    (setq lsp-ui-peek--buffer nil
          lsp-ui-peek--last-xref nil)
    (set-window-start (get-buffer-window) lsp-ui-peek--win-start))

  (advice-add #'lsp-ui-peek--peek-new :override #'lsp-ui-peek--peek-display)
  (advice-add #'lsp-ui-peek--peek-hide :override #'lsp-ui-peek--peek-destroy)

  ;; `C-g'to close doc
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)

  ;; Reset `lsp-ui-doc-background' after loading theme
  (add-hook 'after-load-theme-hook
            (lambda ()
              (setq lsp-ui-doc-border (face-foreground 'font-lock-comment-face nil t))
              (set-face-background 'lsp-ui-doc-background (face-background 'tooltip nil t)))))

;; - https://github.com/tigersoldier/company-lsp
(use-package company-lsp
  :load-path "localelpa/company-lsp"
  :after (lsp company-mode)
  :custom
  (company-lsp-enable-snippet t)        ; 开启 yasnippet 支持
  ;; Disable client-side cache because the LSP server does a better job.
  (company-transformers nil)
  (company-lsp-async t)
  (company-lsp-cache-candidates nil)
  :config
  (with-eval-after-load 'company
    (push 'company-lsp company-backends)))

;;================================================================================
(use-package sideline-lsp
  :disabled t
  :hook (lsp-mode . sideline-mode))

(use-package lsp-ivy
  :disabled t
  :after lsp-mode
  :bind
  (:map lsp-mode-map
        ([remap xref-find-apropos] . lsp-ivy-workspace-symbol)
        ("s-l g A" . lsp-ivy-global-workspace-symbol))
  :config
  (with-no-warnings
    (defvar lsp-ivy-symbol-kind-icons
      `(,(all-the-icons-material "find_in_page" :height 0.9 :v-adjust -0.15)                     ; Unknown - 0
        ,(all-the-icons-faicon "file-o" :height 0.9 :v-adjust -0.02)                             ; File - 1
        ,(all-the-icons-material "view_module" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-lblue) ; Module - 2
        ,(all-the-icons-material "view_module" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue) ; Namespace - 3
        ,(all-the-icons-octicon "package" :height 0.9 :v-adjust -0.15)                                  ; Package - 4
        ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-orange) ; Class - 5
        ,(all-the-icons-faicon "cube" :height 0.9 :v-adjust -0.02 :face 'all-the-icons-purple) ; Method - 6
        ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02)                           ; Property - 7
        ,(all-the-icons-octicon "tag" :height 0.95 :v-adjust 0 :face 'all-the-icons-lblue)     ; Field - 8
        ,(all-the-icons-faicon "cube" :height 0.9 :v-adjust -0.02 :face 'all-the-icons-lpurple) ; Constructor - 9
        ,(all-the-icons-material "storage" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-orange) ; Enum - 10
        ,(all-the-icons-material "share" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-lblue)    ; Interface - 11
        ,(all-the-icons-faicon "cube" :height 0.9 :v-adjust -0.02 :face 'all-the-icons-purple)      ; Function - 12
        ,(all-the-icons-octicon "tag" :height 0.95 :v-adjust 0 :face 'all-the-icons-lblue)          ; Variable - 13
        ,(all-the-icons-faicon "cube" :height 0.9 :v-adjust -0.02 :face 'all-the-icons-purple)      ; Constant - 14
        ,(all-the-icons-faicon "text-width" :height 0.9 :v-adjust -0.02)                            ; String - 15
        ,(all-the-icons-material "format_list_numbered" :height 0.95 :v-adjust -0.15)               ; Number - 16
        ,(all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue)         ; Boolean - 17
        ,(all-the-icons-material "view_array" :height 0.95 :v-adjust -0.15)                         ; Array - 18
        ,(all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-blue)          ; Object - 19
        ,(all-the-icons-faicon "key" :height 0.9 :v-adjust -0.02)                                   ; Key - 20
        ,(all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0)                                    ; Null - 21
        ,(all-the-icons-material "format_align_right" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue) ; EnumMember - 22
        ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-orange) ; Struct - 23
        ,(all-the-icons-octicon "zap" :height 0.9 :v-adjust 0 :face 'all-the-icons-orange) ; Event - 24
        ,(all-the-icons-material "control_point" :height 0.9 :v-adjust -0.15)              ; Operator - 25
        ,(all-the-icons-faicon "arrows" :height 0.9 :v-adjust -0.02)                       ; TypeParameter - 26
        ))

    (lsp-defun my-lsp-ivy--format-symbol-match
               ((sym &as &SymbolInformation :kind :location (&Location :uri))
                project-root)
               "Convert the match returned by `lsp-mode` into a candidate string."
               (let* ((sanitized-kind (if (< kind (length lsp-ivy-symbol-kind-icons)) kind 0))
                      (type (elt lsp-ivy-symbol-kind-icons sanitized-kind))
                      (typestr (if lsp-ivy-show-symbol-kind (format "%s " type) ""))
                      (pathstr (if lsp-ivy-show-symbol-filename
                                   (propertize (format " · %s" (file-relative-name (lsp--uri-to-path uri) project-root))
                                               'face font-lock-comment-face)
                                 "")))
                 (concat typestr (lsp-render-symbol-information sym ".") pathstr)))
    (advice-add #'lsp-ivy--format-symbol-match :override #'my-lsp-ivy--format-symbol-match)))

;; (use-package lsp-clients
;;   :ensure nil
;;   :init
;;   (setq lsp-clients-python-library-directories '("/usr/local/" "/usr/")))

;; https://github.com/emacs-lsp/lsp-sonarlint
(use-package lsp-sonarlint
  :disabled t
  :custom
  (lsp-sonarlint-html-enabled t)
  (lsp-sonarlint-java-enabled t)
  (lsp-sonarlint-xml-enabled t)
  (lsp-sonarlint-python-enabled t)
  (lsp-sonarlint-javascript-enabled t))
;;================================================================================

(provide 'init-lsp)
;;; init-lsp.el ends here
