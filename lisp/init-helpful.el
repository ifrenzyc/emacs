;; init-helpful.el --- Initialize helpful settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code

(use-package info
  :general
  (Info-mode-map
   "h" 'Info-backward-node    ; h was Info-help, Info-backward-node was [
   "l" 'Info-forward-node     ; l was Info-history-back, Info-forward-node was ]
   "y" 'Info-help             ; y wasn't bound, Info-help was h
   "K" 'Info-history          ; K wasn't bound, Info-history was L
   "H" 'Info-history-back     ; H was describe-mode, Info-history-back was l
   "L" 'Info-history-forward  ; L was Info-history, Info-history-forward was r
   "k" 'Info-up               ; k wasn't bound, Info-up was ^ and u
   "j" 'Info-menu             ; j was bmkp-info-jump, Info-menu was m
   "b" 'bmkp-info-jump        ; b was beginning-of-buffer, bmkp-info-jump was j
   "?" 'hydra-info/body)
  :config
  (set-face-attribute 'Info-quoted nil
                      :family 'unspecified
                      :inherit font-lock-type-face)

  (defhydra hydra-info (:color pink
                        :hint nil)
    "
  Info-mode:
  _I_ndex(virtual)    _T_OC                            ^ ^^ ^  ^ ^ ^^     _k_/_u_p   ( )
  _i_ndex             _t_op node        Node           _[__h_ + _l__]_      _j_/_m_enu ( ) (C-u for new window)
  _c_opy node name    _a_propos         Top/Final Node _<__t_   ^ ^_>_      _g_oto node^^    (C-u for new window)
  _C_lone buffer      _f_ollow          Level nxt/prev _p_^ ^   ^ ^_n_
  _d_irectory         _b_mkp-jump       History        _H_^ ^   ^ ^_L_      _K_ History^^

  _s_earch regex (_S_ case sens) ^^^^   _1_ .. _9_ Pick first .. ninth item in the node's menu.
  "
    ("j"   Info-menu)              ;; m
    ("k"   Info-up)                ;; ^
    ("m"   Info-menu)
    ("u"   Info-up)

    ("l"   Info-forward-node)
    ("h"   Info-backward-node)
    ("]"   Info-forward-node)
    ("["   Info-backward-node)

    ("t"   Info-top-node)
    ("<"   Info-top-node)
    (">"   Info-final-node)

    ("n"   Info-next)
    ("p"   Info-prev)

    ("K"   Info-history)
    ("H"   Info-history-back)
    ("L"   Info-history-forward)

    ("s"   Info-search)
    ("S"   Info-search-case-sensitively)

    ("g"   Info-goto-node)

    ("f"   Info-follow-reference)
    ("b"   bmkp-info-jump)
    ("i"   Info-index)
    (","   Info-index-next)
    ("I"   Info-virtual-index)

    ("T"   Info-toc)
    ("t"   Info-top-node)
    ("d"   Info-directory)
    ("c"   Info-copy-current-node-name)
    ("C"   clone-buffer)
    ("a"   info-apropos)

    ("1"   Info-nth-menu-item)
    ("2"   Info-nth-menu-item)
    ("3"   Info-nth-menu-item)
    ("4"   Info-nth-menu-item)
    ("5"   Info-nth-menu-item)
    ("6"   Info-nth-menu-item)
    ("7"   Info-nth-menu-item)
    ("8"   Info-nth-menu-item)
    ("9"   Info-nth-menu-item)

    ("?"   Info-summary "Info summary")
    ("y"   Info-help "Info help")
    ("q"   Info-exit "Info exit" :color blue)
    ("C-g" nil "cancel" :color blue)))

;; makes manual pages nicer to look at :) Variable pitch fontification + colouring
(use-package info-colors)

(use-package helpful
  :pretty-hydra
  ((:color teal :quit-key "q")
   ("Helpful"
    (("f" helpful-callable "callable")
     ("v" helpful-variable "variable")
     ("k" helpful-key "key")
     ("m" describe-mode "mode")
     ("c" helpful-command "command")
     ("d" helpful-at-point "thing at point"))))
  :bind ("C-h" . helpful-hydra/body))

(provide 'init-helpful)
