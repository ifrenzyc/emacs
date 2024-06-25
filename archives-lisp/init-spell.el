;; init-spell.el --- Initialize spell settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; - 其他：https://github.com/redguardtoo/wucuo
;; 

;;; Code:

;; (use-package ispell
;;   :config (setq ispell-program-name "aspell" ; use aspell instead of ispell
;;                 ispell-extra-args '("--sug-mode=ultra"))
;;   ;;ispell complete
;;   ;; (setq company-ispell-dictionary (expand-file-name "ac-dict" user-emacs-directory))
;;   )

(use-package flyspell
  :if (executable-find "aspell")
  :commands flyspell-mode
  :hook
  (((text-mode org-mode) . turn-on-flyspell)
   (prog-mode . flyspell-prog-mode)
   (flyspell-mode . (lambda ()
                      (dolist (key '("C-;" "C-," "C-."))
                        (unbind-key key flyspell-mode-map)))))
  :init
  (setq flyspell-issue-message-flag nil
        ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together"))
  ;; :general
  ;; (yc/leader-keys
  ;;   "t M-s" 'my/toggle-flyspell
  ;;   "M-s"   '(:ignore t :wk "Spell Check")
  ;;   "M-s b" 'flyspell-buffer
  ;;   "M-s n" 'flyspell-goto-next-error
  ;;   "M-s p" 'flyspell-correct-at-point
  ;;   "M-s ." 'hydra-spelling/body)
  :config
  (defhydra hydra-spelling ()
    ("b" flyspell-buffer "check buffer")
    ("d" ispell-change-dictionary "change dictionary")
    ("n" flyspell-goto-next-error "next")
    ("c" flyspell-correct-previous-word-generic "correct")
    ("q" nil "quit"))
  
  (defun my/toggle-flyspell ()
    (interactive)
    (if (bound-and-true-p flyspell-mode)
        (progn
          (flyspell-mode -1)
          (message "Flyspell mode disabled in current buffer"))
      (progn
        (flyspell-mode 1)
        (message "Flyspell mode enabled in current buffer"))))
  )

(use-package flyspell-correct
  :after flyspell
  ;; :general
  ;; (flyspell-mode-map "C-M-;" 'flyspell-correct-wrapper)
  )

(use-package flyspell-correct-ivy
  :after (flyspell-correct ivy)
  :bind (:map flyspell-mode-map
              ([remap flyspell-correct-word-before-point] . flyspell-correct-wrapper))
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy)
  ;; :general
  ;; (yc/nonprefix-keys
  ;;   "C-;" 'flyspell-correct-wrapper)  ;; 这个按键目前和 avy 有冲突，使用时要调整
  )

;; using a grammar & style checker
(use-package langtool
  :custom
  (langtool-language-tool-jar (expand-file-name "bin/LanguageTool-6.1/languagetool-commandline.jar" user-emacs-directory)))

(use-package langtool-popup)

;; https://github.com/minad/jinx
(use-package jinx
  :ensure-system-package (enchant . "brew install enchant pkgconf")
  :hook
  (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

(provide 'init-spell)
;;; init-spell.el ends here
