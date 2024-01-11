;; lang-golang.el --- Initialize golang settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; - https://arenzana.org/2019/01/emacs-go-mode/
;; 

;;; Code:

(use-package go-mode
  :mode ("\\.go\\'" . go-ts-mode)
  :hook
  (go-ts-mode . flycheck-mode)
  (go-ts-mode . lsp-deferred)
  :general
  ;; (yc/leader-keys-major-mode
  ;;   :keymaps 'go-ts-mode-map
  ;;   ;; "" '(:ignore t :wk "major-mode-cmd")
  ;;   "h" '(:ignore t :wk "help")
  ;;   "hh" 'godoc-at-point
  ;;   "i" '(:ignore t :wk "imports")
  ;;   "ig" 'go-goto-imports
  ;;   "ia" 'go-import-add
  ;;   "ir" 'go-remove-unused-imports
  ;;   "e" '(:ignore t :wk "playground")
  ;;   "eb" 'go-play-buffer
  ;;   "er" 'go-play-region
  ;;   "ed" 'go-download-play
  ;;   "x" '(:ignore t :wk "execute")
  ;;   "xx" 'yc/go-run-main
  ;;   "g" '(:ignore t :wk "goto")
  ;;   "ga" 'ff-find-other-file
  ;;   "gc" 'go-coverage
  ;;   "t" '(:ignore t :wk "test")
  ;;   "tt" 'yc/go-run-test-current-function
  ;;   "ts" 'yc/go-run-test-current-suite
  ;;   "tp" 'yc/go-run-package-tests
  ;;   "tP" 'yc/go-run-package-tests-nested
  ;;   "f" '(:ignore t :wk "guru")
  ;;   "fd" 'go-guru-describe
  ;;   "ff" 'go-guru-freevars
  ;;   "fi" 'go-guru-implements
  ;;   "fc" 'go-guru-peers
  ;;   "fr" 'go-guru-referrers
  ;;   "fj" 'go-guru-definition
  ;;   "fp" 'go-guru-pointsto
  ;;   "fs" 'go-guru-callstack
  ;;   "fe" 'go-guru-whicherrs
  ;;   "f<" 'go-guru-callers
  ;;   "f>" 'go-guru-callees
  ;;   "fo" 'go-guru-set-scope
  ;;   "r" '(:ignore t :wk "rename")
  ;;   "rn" 'go-rename)
  (go-ts-mode-map
   "M-]"        'godef-jump
   "M-["        'pop-tag-mark
   "C-S-F"      'gofmt
   "M-<return>" 'godef-describe)
  :mode-hydra
  (go-ts-mode
   (:title "Go Commands")
   ("Doc"
    (("d" godoc-at-point "doc at point"))
    "Imports"
    (("ia" go-import-add "add")
     ("ir" go-remove-unused-imports "cleanup"))))
  :config
  ;;                (setq go-mode-map
  ;; (let ((m (make-sparse-keymap)))
  ;;   (define-key m "}" #'go-mode-insert-and-indent)
  ;;   (define-key m ")" #'go-mode-insert-and-indent)
  ;;   (define-key m "," #'go-mode-insert-and-indent)
  ;;   (define-key m ":" #'go-mode-insert-and-indent)
  ;;   (define-key m "=" #'go-mode-insert-and-indent)
  ;;   (define-key m (kbd "C-c C-a") #'go-import-add)
  ;;   (define-key m (kbd "C-c C-j") #'godef-jump)
  ;;   ;; go back to point after called godef-jump.  ::super
  ;;   (define-key m (kbd "C-c C-b") #'pop-tag-mark)
  ;;   (define-key m (kbd "C-x 4 C-c C-j") #'godef-jump-other-window)
  ;;   (define-key m (kbd "C-c C-d") #'godef-describe)
  ;;   m))
  (setq compile-command "echo Building... && go build -v && echo Testing... && go test -v && echo Linter... && golint")
  (setq compilation-read-command nil)
  ;;           (add-hook 'go-mode-hook
  ;;                     (lambda ()
  ;;                       (setq tab-width 4)
  ;;                       (setq indent-tabs-mode nil)))

  ;; Quick run current buffer
  (defun yc/go ()
    "run current buffer"
    (interactive)
    (compile (concat "go run " (buffer-file-name))))

  ;; use goimports instead of gofmt ::super
  (setq gofmt-command "goimports")

  (defun yc/go-run-tests (args)
    (interactive)
    (save-selected-window
      (async-shell-command (concat "go test " args))))

  (defun yc/go-run-package-tests ()
    (interactive)
    (yc/go-run-tests ""))

  (defun yc/go-run-package-tests-nested ()
    (interactive)
    (yc/go-run-tests "./..."))

  (defun yc/go-run-test-current-function ()
    (interactive)
    (if (string-match "_test\\.go" buffer-file-name)
        (let ((test-method (if go-use-gocheck-for-testing
                               "-check.f"
                             "-run")))
          (save-excursion
            (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?[[:alnum:]]+)[ ]+\\)?\\(Test[[:alnum:]_]+\\)(.*)")
            (yc/go-run-tests (concat test-method "='" (match-string-no-properties 2) "'"))))
      (message "Must be in a _test.go file to run go-run-test-current-function")))

  (defun yc/go-run-test-current-suite ()
    (interactive)
    (if (string-match "_test\.go" buffer-file-name)
        (if go-use-gocheck-for-testing
            (save-excursion
              (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?\\([[:alnum:]]+\\))[ ]+\\)?Test[[:alnum:]_]+(.*)")
              (yc/go-run-tests (concat "-check.f='" (match-string-no-properties 2) "'")))
          (message "Gocheck is needed to test the current suite"))
      (message "Must be in a _test.go file to run go-test-current-suite")))

  ;; *问题* ：这里需要设置为 ="/opt/homebrew/bin/go"= ，可能应为某些环境变量没有设置成功，暂时还不知道具体哪里没设置，先配置成这样。
  ;; 用上面的 =exec-path-from-shell= 包暂时解决了这个问题

  ;; Run Current File
  ;; - http://ergoemacs.org/emacs/elisp_run_current_file.html
  ;; - https://github.com/grafov/go-playground
  (defun yc/run-current-file ()
    "Execute the current file.
  For example, if the current buffer is x.py, then it'll call「python x.py」in a shell. Output is printed to message buffer.

  The file can be Emacs Lisp, PHP, Perl, Python, Ruby, JavaScript, TypeScript, golang, Bash, Ocaml, Visual Basic, TeX, Java, Clojure.
  File suffix is used to determine what program to run.

  If the file is modified or not saved, save it automatically before run.

  URL `http://ergoemacs.org/emacs/elisp_run_current_file.html'
  Version 2017-07-31"
    (interactive)
    (let (
          ($suffix-map
           ;; (‹extension› . ‹shell program name›)
           `(
             ("php" . "php")
             ("pl" . "perl")
             ("py" . "python")
             ("py3" . ,(if (string-equal system-type "windows-nt") "c:/Python32/python.exe" "python3"))
             ("rb" . "ruby")
             ("go" . "/opt/homebrew/bin/go run")
             ("hs" . "runhaskell")
             ("js" . "node") ; node.js
             ("ts" . "tsc --alwaysStrict --lib DOM,ES2015,DOM.Iterable,ScriptHost --target ES5") ; TypeScript
             ("sh" . "bash")
             ("clj" . "java -cp /home/xah/apps/clojure-1.6.0/clojure-1.6.0.jar clojure.main")
             ("rkt" . "racket")
             ("ml" . "ocaml")
             ("vbs" . "cscript")
             ("tex" . "pdflatex")
             ("latex" . "pdflatex")
             ("java" . "javac")
             ;; ("pov" . "/opt/homebrew/bin/povray +R2 +A0.1 +J1.2 +Am2 +Q9 +H480 +W640")
             ))
          $fname
          $fSuffix
          $prog-name
          $cmd-str)
      (when (not (buffer-file-name)) (save-buffer))
      (when (buffer-modified-p) (save-buffer))
      (setq $fname (buffer-file-name))
      (setq $fSuffix (file-name-extension $fname))
      (setq $prog-name (cdr (assoc $fSuffix $suffix-map)))
      (setq $cmd-str (concat $prog-name " \""   $fname "\""))
      (cond
       ((string-equal $fSuffix "el") (load $fname))
       ((string-equal $fSuffix "go")
        (when (fboundp 'gofmt)
          (gofmt)
          (shell-command $cmd-str "*xah-run-current-file output*" )))
       ((string-equal $fSuffix "java")
        (progn
          (shell-command $cmd-str "*xah-run-current-file output*" )
          (shell-command
           (format "java %s" (file-name-sans-extension (file-name-nondirectory $fname))))))
       (t (if $prog-name
              (progn
                (message "Running…")
                (shell-command $cmd-str "*xah-run-current-file output*" ))
            (message "No recognized program file suffix for this file."))))))

  (defun yc/go-run-main ()
    (interactive)
    (shell-command
     (format "go run %s"
             (shell-quote-argument (buffer-file-name))))))

(use-package company-go
  :after (go-ts-mode company-mode)
  :config
  ;; 加了这段代码，输入 fmt. 会自动显示这个模块相关的函数
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-go)))

(use-package go-direx
  :after go-ts-mode
  :general
  (go-mode-map
   "C-c C-j" 'go-direx-pop-to-buffer))

(use-package go-eldoc
  :after go-ts-mode
  :if (executable-find "gocode")
  :commands (go-eldoc-setup)
  :hook (go-ts-mode . go-eldoc-setup))

;;  (use-package go-complete
;;    :after go-mode)

(use-package go-errcheck
  :after go-ts-mode)

(use-package go-gopath
  :after go-ts-mode)

(use-package go-impl)

(use-package go-projectile
  :after go-ts-mode projectile)

(use-package go-snippets
  :after (go-ts-mode)
  :init (go-snippets-initialize))

(use-package go-rename
  :after go-ts-mode)

(provide 'lang-golang)
;;; lang-golang.el ends here
