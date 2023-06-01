;; lang-java.el --- Initialize java configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; Java 环境设置参考这个：https://searchcode.com/codesearch/view/87114678/
;; - https://github.com/google/styleguide/blob/gh-pages/google-c-style.el
;; 

;;; Code

(use-package java-mode
  :ensure nil
  :mode ("\\.java\\'" . java-ts-mode)
  :hook
  (java-ts-mode . (lambda()
                    (setq-local c-basic-offset 4)
                    (subword-mode)
                    ;; (google-set-c-style)
                    ;; (google-make-newline-indent)
                    ))
  ;; :init
  ;; (add-to-list 'load-path (expand-file-name "localelpa/google-c-style" user-emacs-directory))
  ;; (require 'google-c-style)
  )

;; - https://xpressrazor.wordpress.com/2020/11/04/java-programming-in-emacs/
;; - https://github.com/emacs-lsp/lsp-java
(use-package lsp-java
  :after java-ts-mode
  ;; :mode ("\\.java\\'" . java-mode)
  :init
  (setq-local lsp-ui-doc-enable t
              lsp-headerline-breadcrumb-enable t
              lsp-signature-auto-activate t)

  ;; (defvar java-home "/usr/local/opt/java/current")
  ;; (defvar m2-home "/usr/local/opt/maven/current")
  ;; (setenv "JAVA_HOME" java-home)
  ;; (setenv "M2_HOME" m2-home)
  ;; (setenv "PATH" (concat (getenv "PATH") (format ":%s/bin:%s/bin" java-home m2-home)))
  
  ;; (require 'lsp-java-boot)
  (setq lsp-java-inhibit-message t
        ;; Currently (2019-04-24), dap-mode works best with Oracle
        ;; JDK, see https://github.com/emacs-lsp/dap-mode/issues/31
        ;; 这里为什么指定 java, 参考这个 issues(https://github.com/eclipse/eclipse.jdt.ls/pull/1509), 需要 java 11 以上
        ;; lsp-java-java-path "/usr/local/opt/java/bin/java"
        lsp-java-java-path "/Library/Java/JavaVirtualMachines/jdk-20.jdk/Contents/Home/bin/java"
        ;; lsp-java-java-path "/usr/bin/java"
        ;; Use Google style formatting by default
        lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"
        lsp-java-format-settings-profile "GoogleStyle"

        ;; http://download.eclipse.org/che/che-ls-jdt/snapshots/che-jdt-language-server-latest.tar.gz
        lsp-java-jdt-download-url "http://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz"
        ;; lsp-java-server-install-dir (expand-file-name "eclipse.jdt.ls/server/" user-emacs-directory)
        ;; lsp-java-workspace-dir (expand-file-name "eclipse.jdt.ls/workspace/" user-emacs-directory)

        ;; Don't organise imports on save
        lsp-java-save-actions-organize-imports nil

        lsp-java-maven-download-sources t
        ;; lsp-java-references-code-lens-enabled t
        ;; lsp-java-implementations-code-lens-enabled t

        lsp-java-progress-reports-enabled t

        ;; lsp-java-autobuild-enabled nil
        ;; lsp-java-import-maven-enabled nil
        ;; lsp-java-import-gradle-enabled nil
        ;; lsp-java-max-concurrent-builds 1
        ;; lsp-java-format-on-type-enabled nil
        ;; lsp-java-completion-guess-arguments t
        
        ;; (setq lsp-java-vmargs '("-XX:+UseAdaptiveSizePolicy" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Xmx8G" "-Xms2G" "-Xverify:none" "-jar"))
        lsp-java-vmargs
        (list
         ;; "-XX:+UseParallelGC"
         ;; "-XX:GCTimeRatio=4"
         ;; "-XX:AdaptiveSizePolicyWeight=90"
         ;; "-Dsun.zip.disableMemoryMapping=true"
         ;; "-XX:+UseG1GC"
         "-XX:+UseZGC"
         "-Xmx4G"
         "-Dlog.level=INFO"
         ;; "--illegal-access=permit"
         "-XX:+UseStringDeduplication"
         "-javaagent:/Users/yangc/.m2/repository/org/projectlombok/lombok/1.18.24/lombok-1.18.24.jar"
         )
        )
  :hook
  ((java-mode . lsp-deferred)
   (java-ts-mode . lsp-deferred)
   ;; (java-mode . lsp)
   ;; (java-mode . lsp-java-lens-mode)
   ;; (java-mode . lsp-java-boot-lens-mode)
   ;; (java-mode . lsp-java-boot-lens-mode)
   )
  :general
  (java-ts-mode-map
   "C-c C-f" 'lsp-format-buffer)
  ;; (yc/leader-keys-major-mode
  ;;   :keymaps 'java-mode-map
  ;;   "r"   'ggtags-find-tag-dwim
  ;;   "c"   '(:ignore t :which-key "Run/Compile")
  ;;   "cd"  'dap-java-debug
  ;;   "l"   '(:ignore t :which-key "lsp")
  ;;   "lm"  'lsp-ui-imenu
  ;;   "la"  'lsp-find-definition
  ;;   "lb"  'lsp-find-references
  ;;   "lh"  'lsp-ui-doc-show
  ;;   "le"  'lsp-goto-implementation
  ;;   "lr"  'lsp-goto-type-definition
  ;;   "li"  'lsp-java-organize-imports
  ;;   "ld"  'lsp-describe-thing-at-point
  ;;   "lf"  'lsp-format-buffer
  ;;   "lr"  'lsp-format-region
  ;;   "la"  'lsp-java-add-import
  ;;   "lt"  'lsp-java-add-throws
  ;;   "lc"  'lsp-java-create-field
  ;;   "ll"  'lsp-java-create-local
  ;;   "lp"  'lsp-java-create-parameter
  ;;   "ls"  'lsp-java-spring-initializr
  ;;   "lx"  'lsp-java-extract-to-constant
  ;;   "lu"  'lsp-java-add-unimplemented-methods
  ;;   "lv"  'lsp-java-extract-to-local-variable
  ;;   "lg"  'lsp-java-generate-getters-and-setters
  ;;   "lu"  'lsp-java-generate-equals-and-hash-code)
  )

(use-package dap-java
  :ensure nil
  :after (lsp-java dap)
  ;; :config
  ;; (global-set-key (kbd "<f7>") 'dap-step-in)
  ;; (global-set-key (kbd "<f8>") 'dap-next)
  ;; (global-set-key (kbd "<f9>") 'dap-continue)
  )

(use-package meghanada
  :disabled t
  :init (setq meghanada-server-install-dir (concat yc/cache-dir "meghanada"))
  :hook
  (java-ts-mode . (lambda ()
                    ;; meghanada-mode on
                    (meghanada-mode t)
                    (flycheck-mode +1)
                    ;; use code format
                    ;; (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)
                    ))
  :config
  (setq meghanada-use-eldoc t
        meghanada-use-auto-start t
        meghanada-java-path "java"
        meghanada-maven-path "mvn"))

;; A nice collection of stealable Java snippets:
(use-package java-snippets
  :after yasnippet)

;; decompile class file
(use-package autodisass-java-bytecode
  :disabled t)

;; Gradle
(use-package gradle-mode
  :mode ("\\.gradle\\'". gradle-mode))

;; https://writequit.org/articles/working-with-logs-in-emacs.html
(use-package log4j-mode
  :hook
  (log4j-mode . view-mode)
  (log4j-mode . read-only-mode))

(provide 'lang-java)
