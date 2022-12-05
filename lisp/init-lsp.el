;; init-lsp.el --- Initialize lsp settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; - https://github.com/emacs-lsp/lsp-mode
;; 
;; - lsp-mode 的优化提速 :: http://blog.binchen.org/posts/how-to-speed-up-lsp-mode.html
;; 
;; 解决 macOS 下，使用 nvm 管理 nodejs，在执行 (lsp-install-server) 时报错 =env: node: No such file or directory=，原因应该是 lsp 在系统路径上找 node 和 npm 没找到
;; ln -s ~/.nvm/versions/node/v14.11.0/bin/npm /usr/local/bin/npm
;; ln -s ~/.nvm/versions/node/v14.11.0/bin/node /usr/local/bin/node
;; 

;;; Code
(use-package lsp-mode
  ;; :demand t
  :commands (lsp lsp-deferred)
  :hook
  ((lsp . lsp-lens-mode)
   ;; (prog-mode . lsp-deferred)
   (lsp-managed-mode . (lambda ()
                         (with-eval-after-load 'company
                           (setq-local company-backends '((company-capf :with company-yasnippet))))))
   (lsp-mode . lsp-enable-which-key-integration))
  ;; (lsp-after-open . lsp-enable-imenu)
  :general
  (yc/leader-keys
    "l" '(:keymap lsp-command-map :package lsp-mode :wk "lsp"))
  (lsp-command-map
   "d" 'lsp-find-definition
   "f" 'lsp-find-references
   "H" 'lsp-ui-doc-show
   "i" 'lsp-goto-implementation
   "t" 'lsp-goto-type-definition
   "s" 'lsp-ivy-workspace-symbol
   "S" 'lsp-ivy-global-workspace-symbol)
  (lsp-mode-map
   "C-c C-d" 'lsp-describe-thing-at-point
   "C-c l" 'hydra-lsp-map/body)
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
  (lsp-headerline-breadcrumb-enable-diagnostics nil)
  (flymake-fringe-indicator-position 'right-fringe)
  (lsp-signature-function 'lsp-signature-posframe)
  (lsp-eldoc-enable-hover nil)

  (lsp-completion-provider :none) ;; we use Corfu!

  (exec-path (append exec-path '("~/.nvm/versions/node/v18.12.1/bin/")))
  (lsp-sqls-server "/usr/local/opt/go/libexec/bin/sqls")

  ;; @see https://emacs-lsp.github.io/lsp-mode/page/performance
  (read-process-output-max (* 1024 1024)) ;; 1mb
  ;; ;; Performance tweaks, see
  ;; ;; https://github.com/emacs-lsp/lsp-mode#performance
  ;; (setq gc-cons-threshold 100000000
  ;;       ;; lsp-file-watch-threshold 1000
  ;;       lsp-enable-semantic-highlighting t
  ;;       lsp-idle-delay 0.500
  ;;       lsp-diagnostics-modeline-scope :project    ;; :project/:workspace/:file
  ;; (add-hook 'lsp-managed-mode-hook 'lsp-diagnostics-modeline-mode)
  ;; :custom-face
  ;; (lsp-headerline-breadcrumb-path-error-face
  ;;  ((t :underline (:style wave :color ,(face-foreground 'error))
  ;;      :inherit lsp-headerline-breadcrumb-path-face)))
  ;; (lsp-headerline-breadcrumb-path-warning-face
  ;;  ((t :underline (:style wave :color ,(face-foreground 'warning))
  ;;      :inherit lsp-headerline-breadcrumb-path-face)))
  ;; (lsp-headerline-breadcrumb-path-info-face
  ;;  ((t :underline (:style wave :color ,(face-foreground 'success))
  ;;      :inherit lsp-headerline-breadcrumb-path-face)))
  ;; (lsp-headerline-breadcrumb-path-hint-face
  ;;  ((t :underline (:style wave :color ,(face-foreground 'success))
  ;;      :inherit lsp-headerline-breadcrumb-path-face)))
  ;; (lsp-headerline-breadcrumb-symbols-error-face
  ;;  ((t :inherit lsp-headerline-breadcrumb-symbols-face
  ;;      :underline (:style wave :color ,(face-foreground 'error)))))
  ;; (lsp-headerline-breadcrumb-symbols-warning-face
  ;;  ((t :inherit lsp-headerline-breadcrumb-symbols-face
  ;;      :underline (:style wave :color ,(face-foreground 'warning)))))
  ;; (lsp-headerline-breadcrumb-symbols-info-face
  ;;  ((t :inherit lsp-headerline-breadcrumb-symbols-face
  ;;      :underline (:style wave :color ,(face-foreground 'success)))))
  ;; (lsp-headerline-breadcrumb-symbols-hint-face
  ;;  ((t :inherit lsp-headerline-breadcrumb-symbols-face
  ;;      :underline (:style wave :color ,(face-foreground 'success)))))
  :init
  (setq lsp-semantic-tokens-enable t
        lsp-progress-spinner-type 'horizontal-breathing)
  (advice-add 'lsp :before (lambda (&rest _args) (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))
  :config
  (defhydra hydra-lsp (:exit t :hint nil)
    "
   Buffer^^               Server^^                   Symbol
  -------------------------------------------------------------------------------------
   [_f_] format           [_M-r_] restart            [_d_] declaration       [_i_] implementation        [_o_] documentation
   [_m_] imenu            [_S_]   shutdown           [_D_] peek definition   [_I_] peek implementation   [_r_] rename
   [_x_] execute action   [_M-s_] describe session   [_R_] peek references   [_t_] type                  [_s_] signature"
    ("d" lsp-find-declaration)
    ("D" lsp-ui-peek-find-definitions)
    ("R" lsp-ui-peek-find-references)
    ("I" lsp-ui-peek-find-implementation)
    ("i" lsp-find-implementation)
    ("t" lsp-find-type-definition)
    ("s" lsp-signature-help)
    ("o" lsp-describe-thing-at-point)
    ("r" lsp-rename)

    ("f" lsp-format-buffer)
    ("m" lsp-ui-imenu)
    ("x" lsp-execute-code-action)

    ("M-s" lsp-describe-session)
    ("M-r" lsp-restart-workspace)
    ("S" lsp-shutdown-workspace))

  (pretty-hydra-define hydra-lsp-new-2
    (:hint nil :color blue :foreign-keys warn :quit-key "q")
    ("Xref"
     (("d" xref-find-definitions "Definitions")
      ("D" xref-find-definitions-other-window "-> other win")
      ("r" xref-find-references "References")
      ("s" netrom/helm-lsp-workspace-symbol-at-point "Helm search")
      ("S" netrom/helm-lsp-global-workspace-symbol-at-point "Helm global search"))
     "Peek"
     (("C-d" lsp-ui-peek-find-definitions "Definitions")
      ("C-r" lsp-ui-peek-find-references "References")
      ("C-i" lsp-ui-peek-find-implementation "Implementation"))
     "LSP"
     (("p" lsp-describe-thing-at-point "Describe at point")
      ("C-a" lsp-execute-code-action "Execute code action")
      ("R" lsp-rename "Rename")
      ("t" lsp-goto-type-definition "Type definition")
      ("i" lsp-goto-implementation "Implementation")
      ("f" helm-imenu "Filter funcs/classes (Helm)")
      ("F" helm-imenu-in-all-buffers "-> in all buffers")
      ("C-c" lsp-describe-session "Describe session"))
     "Flycheck"
     (("l" lsp-ui-flycheck-list "List errs/warns/notes"))
     "quit"
     (("q" nil "Quit"))))

  (pretty-hydra-define hydra-lsp-new
    (:hint nil :color blue :foreign-keys warn :quit-key "q")
    ("definitions"
     (("d" lsp-ui-peek-find-definitions "peek definition")
      ("D" xref-find-definitions "xref definitions"))
     "references"
     (("r" lsp-ui-peek-find-references "peek references")
      ("R" xref-find-references "xref references")
      ("u" lsp-treemacs-references "usages"))
     "refactor"
     (("n" lsp-rename "rename")
      ("=" lsp-format-buffer "format")
      ("a" lsp-ui-sideline-apply-code-actions "apply code action"))
     "info"
     (("o" lsp-treemacs-symbols "outline"))
     "errors"
     (("e" lsp-treemacs-errors-list "list"))
     "workspace"
     (("i" lsp-describe-session "session info")
      ("S" lsp-restart-workspace "restart workspace"))
     "folders"
     (("w a" lsp-workspace-folders-add "add folder" :column "workspace")
      ("w r" lsp-workspace-folders-remove "remove folder")
      ("w s" lsp-workspace-folders-switch "switch folder"))
     "quit"
     (("q" nil "Quit"))))
  (pretty-hydra-define hydra-lsp-map
    (:title (pretty-hydra-title "LSP UI" 'faicon "rocket" :face 'all-the-icons-green)
            :hint nil :color blue :quit-key "q")
    ("Formatting"
     (("= =" lsp-format-buffer "format buffer")
      ("= r" lsp-format-region "format region"))
     "Actions"
     (("a a" lsp-execute-code-action "code actions")
      ("a h" lsp-document-highlight "highlight symbol")
      ("a l" lsp-avy-lens "lens"))
     "Folders"
     (("F a" lsp-workspace-folders-add "add folder")
      ("F b" lsp-workspace-blacklist-remove "un-blacklist folder")
      ("F r" lsp-workspace-folders-remove "remove folder"))
     "Peeks"
     (("G g" lsp-ui-peek-find-definitions "peek definitions")
      ("G i" lsp-ui-peek-find-implementation "peek implementations")
      ("G r" lsp-ui-peek-find-references "peek references")
      ("G s" lsp-ui-peek-find-workspace-symbol "peek workspace symbol"))
     "Goto"
     (("g a" xref-find-apropos "find symbol in workspace")
      ("g d" lsp-find-declaration "find declarations")
      ("g e" lsp-treemacs-errors-list "show errors")
      ("g g" lsp-find-definition "find definitions")
      ("g h" lsp-treemacs-call-hierarchy "call hierarchy")
      ("g i" lsp-find-implementation "find implementations")
      ("g r" lsp-find-references "find references")
      ("g t" lsp-find-type-definition "find type definition"))
     "Help"
     (("h g" lsp-ui-doc-glance "glance symbol")
      ("h h" lsp-describe-thing-at-point "describe symbol at point")
      ("h s" lsp-signature-activate "signature help"))
     "Refactoring"
     (("r o" lsp-organize-imports "organize imports")
      ("r r" lsp-rename "rename"))
     "Toggles"
     (("T D" lsp-modeline-diagnostics-mode "toggle modeline diagnostics")
      ("T L" lsp-toggle-trace-io "toggle log io")
      ("T S" lsp-ui-sideline-mode "toggle sideline")
      ("T T" lsp-treemacs-sync-mode "toggle treemacs integration")
      ("T a" lsp-modeline-code-actions-mode "toggle modeline code actions")
      ("T b" lsp-headerline-breadcrumb-mode "toggle breadcrumb")
      ("T d" lsp-ui-doc-mode "toggle documentation popup")
      ("T f" lsp-toggle-on-type-formatting "toggle on type formatting")
      ("T h" lsp-toggle-symbol-highlight "toggle highlighting")
      ("T l" lsp-lens-mode "toggle lenses")
      ("T s" lsp-toggle-signature-auto-activate "toggle signature"))
     "Workspaces"
     (("w D" lsp-disconnect "disconnect")
      ("w d" lsp-describe-session)
      ("w q" lsp-workspace-shutdown "shutdown server")
      ("w r" lsp-workspace-restart "restart server")
      ("w s" lsp "start server")))))

;; (use-package lsp-clients
;;   :ensure nil
;;   :init
;;   (setq lsp-clients-python-library-directories '("/usr/local/" "/usr/")))

(use-package lsp-ui
  :after lsp-mode
  :custom-face
  (lsp-ui-sideline-code-action ((t (:inherit warning))))
  :pretty-hydra
  ((:title (pretty-hydra-title "LSP UI" 'faicon "rocket" :face 'all-the-icons-green)
           :color amaranth :quit-key "q")
   ("Doc"
    (("d e" (progn
              (lsp-ui-doc-enable (not lsp-ui-doc-mode))
              (setq lsp-ui-doc-enable (not lsp-ui-doc-enable)))
      "enable" :toggle lsp-ui-doc-mode)
     ("d s" (setq lsp-ui-doc-include-signature (not lsp-ui-doc-include-signature))
      "signature" :toggle lsp-ui-doc-include-signature)
     ("d t" (setq lsp-ui-doc-position 'top)
      "top" :toggle (eq lsp-ui-doc-position 'top))
     ("d b" (setq lsp-ui-doc-position 'bottom)
      "bottom" :toggle (eq lsp-ui-doc-position 'bottom))
     ("d p" (setq lsp-ui-doc-position 'at-point)
      "at point" :toggle (eq lsp-ui-doc-position 'at-point))
     ("d h" (setq lsp-ui-doc-header (not lsp-ui-doc-header))
      "header" :toggle lsp-ui-doc-header)
     ("d f" (setq lsp-ui-doc-alignment 'frame)
      "align frame" :toggle (eq lsp-ui-doc-alignment 'frame))
     ("d w" (setq lsp-ui-doc-alignment 'window)
      "align window" :toggle (eq lsp-ui-doc-alignment 'window)))
    "Sideline"
    (("s e" (progn
              (lsp-ui-sideline-enable (not lsp-ui-sideline-mode))
              (setq lsp-ui-sideline-enable (not lsp-ui-sideline-enable)))
      "enable" :toggle lsp-ui-sideline-mode)
     ("s h" (setq lsp-ui-sideline-show-hover (not lsp-ui-sideline-show-hover))
      "hover" :toggle lsp-ui-sideline-show-hover)
     ("s d" (setq lsp-ui-sideline-show-diagnostics (not lsp-ui-sideline-show-diagnostics))
      "diagnostics" :toggle lsp-ui-sideline-show-diagnostics)
     ("s s" (setq lsp-ui-sideline-show-symbol (not lsp-ui-sideline-show-symbol))
      "symbol" :toggle lsp-ui-sideline-show-symbol)
     ("s c" (setq lsp-ui-sideline-show-code-actions (not lsp-ui-sideline-show-code-actions))
      "code actions" :toggle lsp-ui-sideline-show-code-actions)
     ("s i" (setq lsp-ui-sideline-ignore-duplicate (not lsp-ui-sideline-ignore-duplicate))
      "ignore duplicate" :toggle lsp-ui-sideline-ignore-duplicate))
    "Breadcrumb"
    (("b b" (setq lsp-headerline-breadcrumb-enable (not lsp-headerline-breadcrumb-enable))
      "breadcrumb" :toggle lsp-headerline-breadcrumb-enable))
    "Action"
    (("h" backward-char "←")
     ("j" next-line "↓")
     ("k" previous-line "↑")
     ("l" forward-char "→")
     ("C-a" mwim-beginning-of-code-or-line nil)
     ("C-e" mwim-end-of-code-or-line nil)
     ("C-b" backward-char nil)
     ("C-n" next-line nil)
     ("C-p" previous-line nil)
     ("C-f" forward-char nil)
     ("M-b" backward-word nil)
     ("M-f" forward-word nil)
     ("c" lsp-ui-sideline-apply-code-actions "apply code actions"))))
  :bind (("C-c u" . lsp-ui-imenu)
         :map lsp-ui-mode-map
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions) ; [M-.]
         ([remap xref-find-references] . lsp-ui-peek-find-references)   ; [M-?]
         ("M-<f6>" . lsp-ui-hydra/body)
         ("M-RET" . lsp-ui-sideline-apply-code-actions))
  ;; :general
  ;; (yc/leader-keys
  ;;   :keymaps 'lsp-ui-mode-map
  ;;   "jp" '(:ignore t :wk "peek")
  ;;   "jpd" 'lsp-ui-peek-find-definitions
  ;;   "jpr" 'lsp-ui-peek-find-references)
  :hook
  (lsp-mode . lsp-ui-mode)
  (lsp-mode . lsp-ui-sideline-mode)
  :init
  (setq lsp-ui-doc-enable nil
        lsp-ui-doc-header t
        lsp-ui-doc-position 'top ;; at-point
        lsp-ui-doc-delay 0.2     ;; 延迟 2s 再打开 doc 窗口
        ;; lsp-ui-doc-border (face-foreground 'default)
        lsp-ui-doc-border "orange" ;; (face-foreground 'font-lock-comment-face nil t)
        lsp-ui-doc-include-signature t
        lsp-ui-doc-show-with-cursor nil ;; disable cursor hover
        lsp-ui-doc-show-with-mouse nil  ;; disable mouse hover

        lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-show-code-actions nil

        lsp-lens-enable nil

        lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                              ,(face-foreground 'font-lock-string-face)
                              ,(face-foreground 'font-lock-constant-face)
                              ,(face-foreground 'font-lock-variable-name-face)))
  ;; (if (featurep 'xwidget-internal)
  ;;     (setq lsp-ui-doc-use-webkit t))
  ;; (setq-default lsp-ui-doc-frame-parameters
  ;;               '((left . -1)
  ;;                 (top . -1)
  ;;                 (no-accept-focus . t)
  ;;                 (min-width . 0)
  ;;                 (width . 0)
  ;;                 (min-height . 0)
  ;;                 (height . 0)
  ;;                 (internal-border-width . 0)
  ;;                 (vertical-scroll-bars)
  ;;                 (horizontal-scroll-bars)
  ;;                 (left-fringe . 0)
  ;;                 (right-fringe . 8)
  ;;                 (menu-bar-lines . 0)
  ;;                 (tool-bar-lines . 0)
  ;;                 (line-spacing . 0.2)
  ;;                 (unsplittable . t)
  ;;                 (undecorated . t)
  
  ;;                 (visibility . nil)
  ;;                 (mouse-wheel-frame . nil)
  ;;                 (no-other-frame . t)
  ;;                 (cursor-type)
  ;;                 (no-special-glyphs . t)))
  ;; (set-face-attribute 'lsp-ui-sideline-global nil
  ;;                     :inherit 'shadow
  ;;                     :background "#f9f")
  ;; for "Jimx-/lsp-ui" fork has xwebkit support.
  :config
  (setq ;; scroll-margin 0
   lsp-ui-doc-use-webkit (featurep 'xwidget-internal))
  
  ;; https://github.com/emacs-lsp/lsp-ui/issues/441
  (defun lsp-ui-peek--peek-display (src1 src2)
    (-let* ((win-width (frame-width))
            (lsp-ui-peek-list-width (/ (frame-width) 2))
            (string (-some--> (-zip-fill "" src1 src2)
                      (--map (lsp-ui-peek--adjust win-width it) it)
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

(use-package lsp-ivy
  :after lsp-mode
  :bind (:map lsp-mode-map
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

;; (use-package helm-lsp
;;   :after (helm lsp-mode)
;;   :bind (:map lsp-mode-map
;;               ("s-l g s" . helm-lsp-workspace-symbol)
;;               ("s-l g S" . helm-lsp-global-workspace-symbol))

;; https://github.com/emacs-lsp/lsp-sonarlint
(use-package lsp-sonarlint
  :disabled t
  :init
  (setq lsp-sonarlint-html-enabled t)
  (setq lsp-sonarlint-java-enabled t)
  (setq lsp-sonarlint-xml-enabled t)
  (setq lsp-sonarlint-python-enabled t)
  (setq lsp-sonarlint-javascript-enabled t))

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

(provide 'init-lsp)
