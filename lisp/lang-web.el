;; lang-web.el --- Initialize web settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:
(use-package ng2-mode)

(use-package tide
  :hook
  (typescript-mode    . lsp)
  (typescript-ts-mode . lsp)
  ;; (typescript-mode . setup-tide-mode)
  ;; formats the buffer before saving
  ;; (typescript-mode . (lambda ()   ; 保存文件的时候对该源文件做一下 gofmt
  ;;                      (add-hook 'before-save-hook 'tide-format-before-save)))
  :config
  ;; (defun setup-tide-mode ()
  ;;   (interactive)
  ;;   (tide-setup)
  ;;   (flycheck-mode +1)
  ;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
  ;;   (eldoc-mode +1)
  ;;   (tide-hl-identifier-mode +1)
  ;;   ;; company is an optional dependency. You have to
  ;;   ;; install it separately via package-install
  ;;   ;; `M-x package-install [ret] company`
  ;;   (company-mode +1))

  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)
  (setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil)))

(use-package web-mode
  :commands web-mode
  :mode
  (("\\.phtml\\'"     . web-mode)
   ("\\.tpl\\.php\\'" . web-mode)
   ("\\.[agj]sp\\'"   . web-mode)
   ("\\.as[cp]x\\'"   . web-mode)
   ("\\.jsx\\'"       . web-mode)
   ("\\.js\\'"        . web-mode)
   ("\\.erb\\'"       . web-mode)
   ("\\.mustache\\'"  . web-mode)
   ("\\.djhtml\\'"    . web-mode)
   ("\\.html?\\'"     . web-mode)
   ("\\.css?\\'"      . web-mode))
  :config ;; Autocomplete end tag when finished writing opening tag
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-enable-css-colorization t
        web-mode-code-indent-offset 2
        web-mode-auto-close-style 2
        web-mode-indent-style 2)
  ;; fix javascript indent
  ;; http://mbork.pl/2022-03-14_Proper_indentation_after_a_const_in_Emacs
  (setq js--declaration-keyword-re "\\<\\(let\\|var\\)\\>")
  :hook
  (js2-mode . lsp)
  (web-mode . lsp)
  (web-mode . company-mode)
  (web-mode . (lambda ()
                (when (string-equal "jsx" (file-name-extension buffer-file-name))
                  (setup-tide-mode))))
  ;; configure jsx-tide checker to run after your default jsx checker
  ;;(flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)
  )

(use-package emmet-mode
  :hook
  ((web-mode  . emmet-mode)
   (vue-mode  . emmet-mode)
   (sgml-mode . emmet-mode)    ; Auto-start on any markup modes
   (css-mode  . emmet-mode))    ; enable Emmet's css abbreviation.
  )

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :hook
  (js-mode . js2-minor-mode)
  :config
  (setq js-indent-level 4
        typescript-indent-level 4))

(use-package js-comint
  :config
  (defun whitespace-clean-and-compile ()
    (interactive)
    (whitespace-cleanup-all)
    (compile compile-command))

  ;; Configure jshint for JS style checking.
  ;;   - Install: $ npm install -g jshint
  ;;   - Usage: Hit C-cC-u within any emacs buffer visiting a .js file
  (setq jshint-cli "jshint --show-non-errors ")
  (setq compilation-error-regexp-alist-alist
        (cons '(jshint-cli "^\\([a-zA-Z\.0-9_/-]+\\): line \\([0-9]+\\), col \\([0-9]+\\)"
                           1 ;; file
                           2 ;; line
                           3 ;; column
                           )
              compilation-error-regexp-alist-alist))
  (setq compilation-error-regexp-alist (cons 'jshint-cli compilation-error-regexp-alist))

  (add-hook 'js-mode-hook #'(lambda ()
                              (local-set-key "\C-x\C-e" 'eval-last-sexp)
                              (local-set-key "\C-cb"    'js-send-buffer)
                              (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
                              (local-set-key "\C-cl"    'js-load-file-and-go)
                              (local-set-key "\C-c!"    'run-js)
                              (local-set-key "\C-c\C-r" 'js-send-region)
                              (local-set-key "\C-c\C-j" 'js-send-line)
                              (local-set-key "\C-c\C-u" 'whitespace-clean-and-compile)
                              (set (make-local-variable 'compile-command)
                                   (let ((file buffer-file-name)) (concat jshint-cli file)))
                              (set (make-local-variable 'compilation-read-command) nil)
                              ))

  (defun node-repl-comint-preoutput-filter (output)
    "This function fixes the escape issue with node-repl in js-comint.el.
    Heavily adapted from http://www.squidoo.com/emacs-comint (which
    is in emacs/misc/comint_ticker)
    Basically, by adding this preoutput filter to the
    comint-preoutput-filter-functions list we take the output of
    comint in a *js* buffer and do a find/replace to replace the
    ANSI escape noise with a reasonable prompt."
    (if (equal (buffer-name) "*js*")
        (progn
          ;; Uncomment these to debug the IO of the node process
          ;; (setq js-node-output output)
          ;; (message (concat "\n----------\n" output "\n----------\n"))

          ;; Replaced ^ with \^ to indicate that doesn't have to be
          ;; at start of line
          (replace-regexp-in-string
           "\\\[0K" ""
           (replace-regexp-in-string
            "\\\[1G" ""
            (replace-regexp-in-string
             "\\\[0J" ""
             (replace-regexp-in-string
              "\\\[3G" ""
              (replace-regexp-in-string
               "\\\[0G" ""
               (replace-regexp-in-string
                "\\[2C" ""
                (replace-regexp-in-string
                 "\\[0K" ""
                 (replace-regexp-in-string
                  "" "" output))))))))
          )
      output
      )
    )

  (add-hook 'comint-preoutput-filter-functions 'node-repl-comint-preoutput-filter)
  (add-hook 'comint-output-filter-functions 'node-repl-comint-preoutput-filter))

;;================================================================================
(use-package tern
  :disabled t
  :bind
  (:map tern-mode-keymap
        ("M-."        . tern-find-definition)
        ("C-M-."      . tern-find-definition-by-name)
        ("M-,"        . tern-pop-find-definition)
        ("C-c C-r"    . tern-rename-variable)
        ("C-c C-c"    . tern-get-type)
        ("C-c C-d"    . tern-get-docs)
        ("M-<return>" . tern-get-docs))
  :ensure-system-package (tern . "npm install tern -g")
  :commands tern-mode
  :hook (js2-mode . tern-mode))

(use-package company-tern
  :disabled t
  :load-path "localelpa/company-tern"
  :after (tern company)
  :config
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-tern)))
;;================================================================================

(provide 'lang-web)
;;; lang-web.el ends here
