;; lang-rust.el --- Initialize rust settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :commands (rust-mode)
  :config
  ;; install rustfmt using `cargo install rustfmt'
  (when (executable-find "rustfmt")
    (add-hook 'rust-mode-hook
              (lambda ()
                (add-hook 'before-save-hook
                          (lambda ()
                            (rust-format-buffer)) nil t))))
  (autoload 'rust-mode "rust-mode" nil t))

;; Completion and navigation for Rust
(use-package racer
  :after rust-mode
  :hook (rust-mode . racer-mode)
  :init
  (setq racer-rust-src-path "~/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src")
  ;;:config
  ;; (validate-setq racer-rust-src-path (getenv "RUST_SRC_PATH"))
  )

(use-package cargo
  :after (rust-mode)
  :hook (rust-mode . racer-mode))

(use-package rust-playground
  :after (rust-mode)
  :commands (rust-playground))

(use-package toml-mode
  :after (rust-mode)
  :mode ("\\.toml\\'" . toml-mode)
  :hook (rust-mode . cargo-minor-mode))

(use-package company-racer
  :after rust-mode
  :hook ((rust-mode  . racer-mode)
         (racer-mode . eldoc-mode)
         (racer-mode . company-mode))
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-racer))
  :config
  ;; (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (setq company-tooltip-align-annotations t))

;; `lsp-mode' client using the Rust Language Server
(use-package lsp-rust
  :ensure nil
  :after (lsp-mode rust-mode)
  :hook
  (rust-mode . lsp-deferred))

(provide 'lang-rust)
