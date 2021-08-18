;; init-helm.el --- Initialize helm settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code

(use-package helm
  :config
  (helm-mode t)
  (helm-autoresize-mode 1)
  (setq helm-buffers-fuzzy-matching t
        helm-autoresize-mode t
        helm-buffer-max-length 100)
  (set-face-attribute 'helm-selection nil :background "yellow" :foreground "black")
  (set-face-attribute 'helm-source-header nil :height 240)
  ;; Hydra for in Helm
  (defhydra helm-like-unite ()
    "vim movement"
    ("?" helm-help "help")
    ("q" nil "exit")
    ("<SPC>" helm-toggle-visible-mark "mark")
    ("d" helm-buffer-run-kill-persistent "Delete selection")
    ("a" helm-toggle-all-marks "(un)mark all")
    ;; not sure if there's a better way to this
    ("/" (lambda ()
           (interactive)
           (execute-kbd-macro [?\C-s]))
         "search")
    ("v" helm-execute-persistent-action)
    ("g" helm-beginning-of-buffer "top")
    ("h" helm-previous-source)
    ("l" helm-next-source)
    ("G" helm-end-of-buffer "bottom")
    ("j" helm-next-line "down")
    ("k" helm-previous-line "up")
    ("i" nil "cancel"))
  (key-chord-define helm-map "jk" 'helm-like-unite/body)
  :general
  (helm-map
   "C-j"   'helm-next-line
   "C-k"   'helm-previous-line
   "C-h"   'helm-next-source
   "C-S-h" 'describe-key
   "C-l" (kbd "RET")
   [escape] 'helm-keyboard-quit))

;; TODO: https://github.com/abo-abo/swiper-helm

(use-package helm-fuzzier
  :config
  (helm-fuzzier-mode 1)
  (setq helm-mode-fuzzy-match t
        helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t))

(use-package helm-ag)

(use-package swoop)

(use-package helm-swoop
  :after (helm swoop)
  :general
  ("M-i" 'helm-swoop
         "M-I" 'helm-swoop-back-to-last-point)
  (isearch-mode-map "M-i" 'helm-swoop-from-isearch)
  :config
  ;; Save buffer when helm-multi-swoop-edit complete
  (setq helm-multi-swoop-edit-save t)
  ;; Go to the opposite side of line from the end or beginning of line
  (setq helm-swoop-move-to-line-cycle t)
  ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
  (setq helm-swoop-split-direction 'split-window-vertically))

(use-package helm-projectile
  :config
  (helm-projectile-on)
  (setq projectile-indexing-method 'native)
  (setq projectile-enable-caching t))

(use-package helm-descbinds
  :general
  ("C-h b" 'helm-descbinds
           "C-h h" 'helm-descbinds)
  :config
  (defhydra hydra-help (:exit t)
    ;; Better to exit after any command because otherwise helm gets in a
    ;; mess, set hint to nil: written out manually.

    "
Describe        ^^Keys                    ^^Search                    ^^Documentation
---------------------------------------------------------------------------------------
_f_unction        _k_eybinding              _a_propros                  _i_nfo
_p_ackage         _w_here-is                _d_oc strings               _n_: man
_m_ode            _b_: show all bindings    _s_: info by symbol         _h_elm-dash
_v_ariable

"
    ;; Boring help commands...
    ("e" view-echo-area-messages "messages")
    ("l" view-lossage "lossage")
    ("C" describe-coding-system "coding-system")
    ("I" describe-input-method "input-method")


    ;; Documentation
    ("i" info nil)
    ("n" helm-man-woman nil)
    ("h" helm-dash)

    ;; Keybinds
    ("b" describe-bindings nil)
    ("c" describe-key-briefly nil)
    ("k" describe-key nil)
    ("w" where-is nil)

    ;; Search
    ("a" apropos-command nil)
    ("d" apropos-documentation nil)
    ("s" info-lookup-symbol nil)

    ;; Describe
    ("f" describe-function nil)
    ("p" describe-package nil)
    ("m" describe-mode nil)
    ("v" describe-variable nil)
    ("y" describe-syntax nil)

    ;; quit
    ("q" help-quit "quit")))

;; (defhydra hydra-helm (:hint nil :color pink)
;;         "
;;                                                                           ╭──────┐
;;    Navigation   Other  Sources     Mark             Do             Help   │ Helm │
;;   ╭───────────────────────────────────────────────────────────────────────┴──────╯
;;         ^_k_^         _K_       _p_   [_m_] mark         [_v_] view         [_H_] helm help
;;         ^^↑^^         ^↑^       ^↑^   [_t_] toggle all   [_d_] delete       [_s_] source help
;;     _h_ ←   → _l_     _c_       ^ ^   [_u_] unmark all   [_f_] follow: %(helm-attr 'follow)
;;         ^^↓^^         ^↓^       ^↓^    ^ ^               [_y_] yank selection
;;         ^_j_^         _J_       _n_    ^ ^               [_w_] toggle windows
;;   --------------------------------------------------------------------------------
;;         "
;;         ("<tab>" helm-keyboard-quit "back" :exit t)
;;         ("<escape>" nil "quit")
;;         ("\\" (insert "\\") "\\" :color blue)
;;         ("h" helm-beginning-of-buffer)
;;         ("j" helm-next-line)
;;         ("k" helm-previous-line)
;;         ("l" helm-end-of-buffer)
;;         ("g" helm-beginning-of-buffer)
;;         ("G" helm-end-of-buffer)
;;         ("n" helm-next-source)
;;         ("p" helm-previous-source)
;;         ("K" helm-scroll-other-window-down)
;;         ("J" helm-scroll-other-window)
;;         ("c" helm-recenter-top-bottom-other-window)
;;         ("m" helm-toggle-visible-mark)
;;         ("t" helm-toggle-all-marks)
;;         ("u" helm-unmark-all)
;;         ("H" helm-help)
;;         ("s" helm-buffer-help)
;;         ("v" helm-execute-persistent-action)
;;         ("d" helm-persistent-delete-marked)
;;         ("y" helm-yank-selection)
;;         ("w" helm-toggle-resplit-and-swap-windows)
;;         ("f" helm-follow-mode))

(provide 'init-helm)
