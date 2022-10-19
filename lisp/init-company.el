;; init-company.el --- Initialize company settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; - https://company-mode.github.io/
;; company-mode 是 Emacs 的自动完成插件，与 auto-complete 插件功能类似。
;; 这里需要参考 http://emacs.stackexchange.com/questions/10837/how-to-make-company-mode-be-case-sensitive-on-plain-text
;; 解决 company-mode 自动完成是转换为小写的问题，具体原因参考 https://emacs-china.org/t/company/187
;; - company
;; - autocomplete
;; 
;; company-backends 的设置逻辑：https://emacs.stackexchange.com/questions/17537/best-company-backends-lists
;; 

;;; Code:

(require 'init-const)

(use-package company
  :hook (after-init . global-company-mode)
  :bind
  (("C-." . company-complete)
   ;; :map company-mode-map
   ;; ("<backtab>" . company-yasnippet)
   :map company-active-map
   ("<tab>" . company-complete-selection)
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)
   ("C-d" . company-show-doc-buffer)
   ("C-f" . company-filter-candidates)
   ("C-r" . company-search-candidates)
   ("C-/" . counsel-company)
   ("<backtab>" . my-company-yasnippet)
   :map company-search-map
   ("C-p" . company-select-previous)
   ("C-n" . company-select-next)
   )
  :config
  (with-eval-after-load 'evil
    (with-eval-after-load 'company
      (define-key evil-insert-state-map (kbd "C-n") nil)
      (define-key evil-insert-state-map (kbd "C-p") nil)
      (evil-define-key nil company-active-map (kbd "C-n") #'company-select-next)
      (evil-define-key nil company-active-map (kbd "C-p") #'company-select-previous)))
  (defun my-company-yasnippet ()
    "Hide the current completeions and show snippets."
    (interactive)
    (company-cancel)
    (call-interactively 'company-yasnippet))
  ;; Use the tab-and-go frontend.
  ;; Allows TAB to select and complete at the same time.
  ;; (company-tng-configure-default)
  ;; (setq company-frontends
  ;;       '(company-tng-frontend
  ;;         company-pseudo-tooltip-frontend
  ;;         company-echo-metadata-frontend))
  (setq company-minimum-prefix-length 3
        company-show-numbers t
        company-require-match nil
        company-dabbrev-time-limit 0.3
        company-dabbrev-downcase nil                  ; make company-complete care about case
        company-dabbrev-ignore-case nil               ; fix case-sensitive, default is keep-prefix
        company-dabbrev-other-buffers t
        company-echo-delay 0                          ; remove annoying blinking
        company-idle-delay 0.2                        ; decrease delay before autocompletion popup shows
        company-tooltip-align-annotations t
        company-tooltip-limit 20                      ; bigger popup window
        company-tooltip-flip-when-above t
        company-auto-complete-chars nil
        company-auto-complete t
        company-selection-wrap-around t)
  (setq company-backends '((company-capf :with company-yasnippet)
                           (company-dabbrev-code company-keywords company-files)
                           company-dabbrev))

  (setq company-begin-commands '(self-insert-command org-self-insert-command c-electric-lt-gt c-electric-colon)) ; start autocompletion only after typing
  ;; `yasnippet' integration
  (with-no-warnings
    (with-eval-after-load 'yasnippet
      (defun company-backend-with-yas (backend)
        "Add `yasnippet' to company backend."
        (if (and (listp backend) (member 'company-yasnippet backend))
            backend
          (append (if (consp backend) backend (list backend))
                  '(:with company-yasnippet))))

      (defun my-company-enbale-yas (&rest _)
        "Enable `yasnippet' in `company'."
        (setq company-backends (mapcar #'company-backend-with-yas company-backends)))

      (defun my-lsp-fix-company-capf ()
        "Remove redundant `comapny-capf'."
        (setq company-backends
              (remove 'company-backends (remq 'company-capf company-backends))))
      (advice-add #'lsp-completion--enable :after #'my-lsp-fix-company-capf)

      (defun my-company-yasnippet-disable-inline (fun command &optional arg &rest _ignore)
        "Enable yasnippet but disable it inline."
        (if (eq command 'prefix)
            (when-let ((prefix (funcall fun 'prefix)))
              (unless (memq (char-before (- (point) (length prefix)))
                            '(?. ?< ?> ?\( ?\) ?\[ ?{ ?} ?\" ?' ?`))
                prefix))
          (progn
            (when (and (bound-and-true-p lsp-mode)
                       arg (not (get-text-property 0 'yas-annotation-patch arg)))
              (let* ((name (get-text-property 0 'yas-annotation arg))
                     (snip (format "%s (Snippet)" name))
                     (len (length arg)))
                (put-text-property 0 len 'yas-annotation snip arg)
                (put-text-property 0 len 'yas-annotation-patch t arg)))
            (funcall fun command arg))))
      (advice-add #'company-yasnippet :around #'my-company-yasnippet-disable-inline))))

(use-package company-box
  :diminish
  :hook (company-mode . company-box-mode)
  :defines (company-box-icons-all-the-icons-material company-box-icons-all-the-icons-faicon)
  :init (setq company-box-enable-icon t
              company-box-backends-colors nil
              company-box-show-single-candidate t
              company-box-max-candidates 50
              company-box-doc-delay 0.3)
  :config
  (declare-function all-the-icons-faicon 'all-the-icons)
  (declare-function all-the-icons-material 'all-the-icons)
  (declare-function all-the-icons-octicon 'all-the-icons)
  (setq company-box-icons-all-the-icons-faicon
        `((Unknown       . ,(all-the-icons-material "find_in_page"             :height 0.8  :v-adjust -0.15))
          (Text          . ,(all-the-icons-faicon   "text-width"               :height 0.8  :v-adjust -0.02))
          (Method        . ,(all-the-icons-faicon   "cube"                     :height 0.8  :v-adjust -0.02 :face 'all-the-icons-purple))
          (Function      . ,(all-the-icons-faicon   "cube"                     :height 0.8  :v-adjust -0.02 :face 'all-the-icons-purple))
          (Constructor   . ,(all-the-icons-faicon   "cube"                     :height 0.8  :v-adjust -0.02 :face 'all-the-icons-purple))
          (Field         . ,(all-the-icons-octicon  "tag"                      :height 0.85 :v-adjust 0     :face 'all-the-icons-lblue))
          (Variable      . ,(all-the-icons-octicon  "tag"                      :height 0.85 :v-adjust 0     :face 'all-the-icons-lblue))
          (Class         . ,(all-the-icons-material "settings_input_component" :height 0.8  :v-adjust -0.15 :face 'all-the-icons-orange))
          (Interface     . ,(all-the-icons-material "share"                    :height 0.8  :v-adjust -0.15 :face 'all-the-icons-lblue))
          (Module        . ,(all-the-icons-material "view_module"              :height 0.8  :v-adjust -0.15 :face 'all-the-icons-lblue))
          (Property      . ,(all-the-icons-faicon   "wrench"                   :height 0.8  :v-adjust -0.02))
          (Unit          . ,(all-the-icons-material "settings_system_daydream" :height 0.8  :v-adjust -0.15))
          (Value         . ,(all-the-icons-material "format_align_right"       :height 0.8  :v-adjust -0.15 :face 'all-the-icons-lblue))
          (Enum          . ,(all-the-icons-material "storage"                  :height 0.8  :v-adjust -0.15 :face 'all-the-icons-orange))
          (Keyword       . ,(all-the-icons-material "filter_center_focus"      :height 0.8  :v-adjust -0.15))
          (Snippet       . ,(all-the-icons-material "format_align_center"      :height 0.8  :v-adjust -0.15))
          (Color         . ,(all-the-icons-material "palette"                  :height 0.8  :v-adjust -0.15))
          (File          . ,(all-the-icons-faicon   "file-o"                   :height 0.8  :v-adjust -0.02))
          (Reference     . ,(all-the-icons-material "collections_bookmark"     :height 0.8  :v-adjust -0.15))
          (Folder        . ,(all-the-icons-faicon   "folder-open"              :height 0.8  :v-adjust -0.02))
          (EnumMember    . ,(all-the-icons-material "format_align_right"       :height 0.8  :v-adjust -0.15))
          (Constant      . ,(all-the-icons-faicon   "square-o"                 :height 0.8  :v-adjust -0.1))
          (Struct        . ,(all-the-icons-material "settings_input_component" :height 0.8  :v-adjust -0.15 :face 'all-the-icons-orange))
          (Event         . ,(all-the-icons-octicon  "zap"                      :height 0.8  :v-adjust 0     :face 'all-the-icons-orange))
          (Operator      . ,(all-the-icons-material "control_point"            :height 0.8  :v-adjust -0.15))
          (TypeParameter . ,(all-the-icons-faicon   "arrows"                   :height 0.8  :v-adjust -0.02))
          (Template      . ,(all-the-icons-material "format_align_left"        :height 0.8  :v-adjust -0.15))))
  (setq company-box-icons-all-the-icons-material
        `((Unknown       . ,(all-the-icons-material "find_in_page"             :height 0.8 :face 'all-the-icons-purple))
          (Text          . ,(all-the-icons-material "text_fields"              :height 0.8 :face 'all-the-icons-green))
          (Method        . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))
          (Function      . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))
          (Constructor   . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))
          (Field         . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))
          (Variable      . ,(all-the-icons-material "adjust"                   :height 0.8 :face 'all-the-icons-blue))
          (Class         . ,(all-the-icons-material "class"                    :height 0.8 :face 'all-the-icons-red))
          (Interface     . ,(all-the-icons-material "settings_input_component" :height 0.8 :face 'all-the-icons-red))
          (Module        . ,(all-the-icons-material "view_module"              :height 0.8 :face 'all-the-icons-red))
          (Property      . ,(all-the-icons-material "settings"                 :height 0.8 :face 'all-the-icons-red))
          (Unit          . ,(all-the-icons-material "straighten"               :height 0.8 :face 'all-the-icons-red))
          (Value         . ,(all-the-icons-material "filter_1"                 :height 0.8 :face 'all-the-icons-red))
          (Enum          . ,(all-the-icons-material "plus_one"                 :height 0.8 :face 'all-the-icons-red))
          (Keyword       . ,(all-the-icons-material "filter_center_focus"      :height 0.8 :face 'all-the-icons-red))
          (Snippet       . ,(all-the-icons-material "short_text"               :height 0.8 :face 'all-the-icons-red))
          (Color         . ,(all-the-icons-material "color_lens"               :height 0.8 :face 'all-the-icons-red))
          (File          . ,(all-the-icons-material "insert_drive_file"        :height 0.8 :face 'all-the-icons-red))
          (Reference     . ,(all-the-icons-material "collections_bookmark"     :height 0.8 :face 'all-the-icons-red))
          (Folder        . ,(all-the-icons-material "folder"                   :height 0.8 :face 'all-the-icons-red))
          (EnumMember    . ,(all-the-icons-material "people"                   :height 0.8 :face 'all-the-icons-red))
          (Constant      . ,(all-the-icons-material "pause_circle_filled"      :height 0.8 :face 'all-the-icons-red))
          (Struct        . ,(all-the-icons-material "streetview"               :height 0.8 :face 'all-the-icons-red))
          (Event         . ,(all-the-icons-material "event"                    :height 0.8 :face 'all-the-icons-red))
          (Operator      . ,(all-the-icons-material "control_point"            :height 0.8 :face 'all-the-icons-red))
          (TypeParameter . ,(all-the-icons-material "class"                    :height 0.8 :face 'all-the-icons-red))
          (Yasnippet     . ,(all-the-icons-material "short_text"               :height 0.8 :face 'all-the-icons-green))
          (ElispFunction . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))
          (ElispVariable . ,(all-the-icons-material "check_circle"             :height 0.8 :face 'all-the-icons-blue))
          (ElispFeature  . ,(all-the-icons-material "stars"                    :height 0.8 :face 'all-the-icons-orange))
          (ElispFace     . ,(all-the-icons-material "format_paint"             :height 0.8 :face 'all-the-icons-pink))))
  (setq company-box-icons-alist 'company-box-icons-all-the-icons-material)
  (with-no-warnings
    ;; Prettify icons
    (defun my-company-box-icons--elisp (candidate)
      (when (or (derived-mode-p 'emacs-lisp-mode) (derived-mode-p 'lisp-mode))
        (let ((sym (intern candidate)))
          (cond ((fboundp sym) 'Function)
                ((featurep sym) 'Module)
                ((facep sym) 'Color)
                ((boundp sym) 'Variable)
                ((symbolp sym) 'Text)
                (t . nil)))))
    (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp)

    ;; Display borders of child frame
    (setq company-box-doc-frame-parameters '((internal-border-width . 1)
                                             (left-fringe . 10)
                                             (right-fringe . 10)))
    (defun my-company-box-doc--make-buffer (object)
      (let* ((buffer-list-update-hook nil)
             (inhibit-modification-hooks t)
             (string (cond ((stringp object) object)
                           ((bufferp object) (with-current-buffer object (buffer-string))))))
        (when (and string (> (length (string-trim string)) 0))
          (with-current-buffer (company-box--get-buffer "doc")
            (erase-buffer)
            (insert (propertize "\n" 'face '(:height 0.5)))
            (insert string)
            (insert (propertize "\n\n" 'face '(:height 0.5)))
            (setq mode-line-format nil
                  display-line-numbers nil
                  header-line-format nil
                  show-trailing-whitespace nil
                  cursor-in-non-selected-windows nil)
            (current-buffer)))))
    (advice-add #'company-box-doc--make-buffer :override #'my-company-box-doc--make-buffer)

    (defun my-company-box-doc--show (selection frame)
      (cl-letf (((symbol-function 'completing-read) #'company-box-completing-read)
                (window-configuration-change-hook nil)
                (inhibit-redisplay t)
                (display-buffer-alist nil)
                (buffer-list-update-hook nil))
        (-when-let* ((valid-state (and (eq (selected-frame) frame)
                                       company-box--bottom
                                       company-selection
                                       (company-box--get-frame)
                                       (frame-visible-p (company-box--get-frame))))
                     (candidate (nth selection company-candidates))
                     (doc (or (company-call-backend 'quickhelp-string candidate)
                              (company-box-doc--fetch-doc-buffer candidate)))
                     (doc (company-box-doc--make-buffer doc)))
          (let ((frame (frame-local-getq company-box-doc-frame))
                (border-color (face-foreground 'font-lock-comment-face nil t)))
            (unless (frame-live-p frame)
              (setq frame (company-box-doc--make-frame doc))
              (frame-local-setq company-box-doc-frame frame))
            (set-face-background 'internal-border border-color frame)
            (when (facep 'child-frame-border)
              (set-face-background 'child-frame-border border-color frame))
            (company-box-doc--set-frame-position frame)
            (unless (frame-visible-p frame)
              (make-frame-visible frame))))))
    (advice-add #'company-box-doc--show :override #'my-company-box-doc--show)

    (defun my-company-box--display (string on-update)
      "Display the completions."
      (company-box--render-buffer string on-update)

      (let ((frame (company-box--get-frame))
            (border-color (face-foreground 'font-lock-comment-face nil t)))
        (unless frame
          (setq frame (company-box--make-frame))
          (company-box--set-frame frame))
        (company-box--compute-frame-position frame)
        (company-box--move-selection t)
        (company-box--update-frame-position frame)
        (unless (frame-visible-p frame)
          (make-frame-visible frame))
        (company-box--update-scrollbar frame t)
        (set-face-background 'internal-border border-color frame)
        (when (facep 'child-frame-border)
          (set-face-background 'child-frame-border border-color frame)))
      (with-current-buffer (company-box--get-buffer)
        (company-box--maybe-move-number (or company-box--last-start 1))))
    (advice-add #'company-box--display :override #'my-company-box--display)))

;; 在 MacOS 下使用 posframe 时，Emacs 全屏状态下的问题：https://emacs-china.org/t/topic/4662/132
(use-package posframe)

(use-package company-posframe
  :after (company posframe)
  :init
  (when IS-MAC
    ;; don't use the native fullscreen crap
    ;; WORKAROUND: fix blank screen issue on macOS.
    (setq-default ns-use-native-fullscreen nil))
  ;; (setq ns-use-native-fullscreen nil)
  ;; (setq ns-use-fullscreen-animation nil)
  :config
  ;;   (push '(company-posframe-mode . nil)
  ;; 	  desktop-minor-mode-table)
  (setq company-posframe-show-indicator nil
	    company-posframe-show-metadata nil
	    company-posframe-quickhelp-delay nil)
  (company-posframe-mode 1))

;; Popup documentation for completion candidates
;; - https://github.com/expez/company-quickhelp
(use-package company-quickhelp
  :hook (company-mode . company-quickhelp-mode)
  :defines company-quickhelp-delay
  :bind (:map company-active-map
              ([remap company-show-doc-buffer] . company-quickhelp-manual-begin))
  :config
  (setq company-quickhelp-delay 0.5)
  (setq pos-tip-background-color (face-background 'company-tooltip)
        pos-tip-foreground-color (face-foreground 'company-tooltip))
  :bind (:map company-active-map
              ("M-h" . company-quickhelp-manual-begin)))

(use-package company-statistics
  :hook (company-mode . company-statistics-mode))

;; Better sorting and filtering
(use-package company-prescient
  :hook (company-mode . company-prescient-mode))

;; 基于 Company 编写了一个带中文注释的英文补全助手 - Emacs-general - Emacs China
;; - https://emacs-china.org/t/company/6322/186
(use-package company-english-helper
  :load-path "localelpa/company-english-helper"
  :config
  (require 'company-english-helper)
  (setq company-english-helper-fuzz-search-p t))

(provide 'init-company)
