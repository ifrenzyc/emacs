;; init-modeline.el --- Initialize modeline settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code
(use-package doom-modeline
  ;; :after all-the-icons
  :demand t
  :hook
  (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-icon (display-graphic-p)
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon nil
        doom-modeline-buffer-file-name-style 'truncate-except-project
        ;; Whether display icons for buffer states. It respects `doom-modeline-icon'.
        doom-modeline-buffer-state-icon t
        ;; Whether display buffer modification icon. It respects `doom-modeline-icon'
        ;; and `doom-modeline-buffer-state-icon'.
        doom-modeline-buffer-modification-icon t
        doom-modeline-enable-word-count nil
        doom-modeline-lsp t
        doom-modeline-modal-icon t

        doom-modeline-height 30
        doom-modeline-bar-width 3

        ;; Whether display minor modes in mode-line or not.
        doom-modeline-minor-modes t

        doom-modeline-buffer-encoding t

        ;; Whether display the `evil' state icon.
        ;; doom-modeline-evil-state-icon t

        ;; Whether display perspective name or not. Non-nil to display in mode-line.
        doom-modeline-persp-name t
        ;; Whether display icon for persp name. Nil to display a # sign. It respects `doom-modeline-icon'
        doom-modeline-persp-name-icon t
        ;; Whether display environment version or not
        doom-modeline-env-version t
        ;; Or for individual languages
        doom-modeline-env-enable-python t
        doom-modeline-env-enable-ruby t
        doom-modeline-env-enable-perl t
        doom-modeline-env-enable-go t
        doom-modeline-env-enable-elixir t
        doom-modeline-env-enable-rust t

        ;; Change the executables to use for the language version string
        doom-modeline-env-python-executable "python" ; or `python-shell-interpreter'
        doom-modeline-env-ruby-executable "ruby"
        doom-modeline-env-perl-executable "perl"
        doom-modeline-env-go-executable "go"
        doom-modeline-env-elixir-executable "iex"
        doom-modeline-env-rust-executable "rustc"

        ;; What to dispaly as the version while a new one is being loaded
        doom-modeline-env-load-string "..."

        doom-modeline-checker-simple-format t)
  (doom-modeline-mode t))

;; Use fixed pitch where it's sensible
(use-package mixed-pitch)

(use-package minions
  :init
  (minions-mode))

(use-package nyan-mode
  ;; :demand t
  :after doom-modeline
  :init
  (setq nyan-animate-nyancat t
        nyan-wavy-trail t)
  (nyan-mode)
  (nyan-start-animation))

;; TODO try https://github.com/lunaryorn/fancy-battery.el
(use-package battery
  :ensure nil
  :hook
  (after-init . display-battery-mode))

;; - https://github.com/hlissner/emacs-hide-mode-line
(use-package hide-mode-line
  :commands hide-mode-line-mode
  :hook
  ((completion-list-mode
    ;; vterm-mode
    neotree-mode) . hide-mode-line-mode)
  )

(provide 'init-modeline)