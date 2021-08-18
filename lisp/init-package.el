;; init-package.el --- Initialize package settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code

;; - https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org#dotemacs
(use-package paradox
  :commands (paradox-upgrade-packages)
  :config
  (setq-default paradox-column-width-package 27
                paradox-column-width-version 13
                paradox-execute-asynchronously t
                paradox-github-token t
                paradox-hide-wiki-packages t)
  (remove-hook 'paradox--report-buffer-print 'paradox-after-execute-functions))

(use-package auto-package-update
  :config
  (setq auto-package-update-interval 7) ;; in days
  (setq auto-package-update-prompt-before-update t)
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; macOS 系统自身的包管理系统： ~brew~
(use-package system-packages
  :init
  (when (eq system-type 'darwin)
    (add-to-list 'system-packages-supported-package-managers
                 '(brew2 .
                   ((default-sudo . nil)
                    (install . "proxychains4 brew install")
                    (search . "proxychains4 brew search")
                    (uninstall . "brew uninstall")
                    (update . ("proxychains4 brew update" "proxychains4 brew upgrade" "proxychains4 brew cask upgrade"))
                    (clean-cache . "brew cleanup")
                    (log . nil)
                    (get-info . nil)
                    (get-info-remote . nil)
                    (list-files-provided-by . "brew ls --verbose")
                    (verify-all-packages . nil)
                    (verify-all-dependencies . nil)
                    (remove-orphaned . nil)
                    (list-installed-packages . "proxychains4 brew list")
                    (list-installed-packages-all . "proxychains4 brew list")
                    (list-dependencies-of . "proxychains4 brew deps")
                    (noconfirm . nil))))
    (setq system-packages-use-sudo nil)
    (setq system-packages-package-manager 'brew2)))

(defvar jp-system--title (with-faicon "info" "System tools" 1 -0.05))
(pretty-hydra-define hydra-system
    (:hint t :color blue :foreign-keys warn :quit-key "q" :title jp-system--title :separator "═")
  ("System"
   (("q" nil "quit")
    ("r" restart-emacs "Restart Emacs")
    ("Q" kill-emacs "Quit Emacs"))
   "Packages"
   (("p" paradox-list-packages)
    ("P" paradox-upgrade-packages)
    ("b" system-packages-update "Homebrew update"))
   "Processes"
   (("s" list-processes))))

(provide 'init-package)
