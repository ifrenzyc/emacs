;; init-nano-modeline.el --- Initialize nano modeline settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; - 基于 nano-modeline 的 bespoke-modeline
;; - https://github.com/mclear-tools/bespoke-modeline
;; 

;;; Code
(use-package nano-modeline
  ;; :straight (:host github :repo "rougier/nano-modeline")
  :config
  (nano-modeline)
  ;; (advice-add #'nano-dark :after #'nano-modeline)
  (advice-add #'nano-light :after #'nano-modeline))

(provide 'init-nano-modeline)
