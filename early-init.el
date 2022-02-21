;;; early-init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Copyright (c) 2016-2018 Yang Chuang
;;
;; Author: Yang Chuang <ifrenzyc@gmail.com>
;; URL: https://github.com/ifrenzyc
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs27 introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:
;; native-comp
(when (boundp 'native-comp-eln-load-path)
  (setenv "LIBRARY_PATH" "/usr/local/opt/gcc/lib/gcc/11:/usr/local/opt/gcc/lib/gcc/11/gcc/x86_64-apple-darwin21/11")
  (setq comp-async-report-warnings-errors nil)
  (setq native-comp-async-report-warnings-errors nil)
  ;; (add-to-list 'native-comp-eln-load-path (expand-file-name ".cache/eln-cache/" user-emacs-directory))
  (setcar native-comp-eln-load-path
          (expand-file-name ".cache/eln-cache/" user-emacs-directory)))
;; -native-comp

;; DeferGC
;; Garbage collection
;; Set garbage collect high to speed up startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      inhibit-compacting-font-caches t)
;; -DeferGC

;; Increase max-lisp-eval-depth
(setq max-lisp-eval-depth 3200)
(setq max-specpdl-size 5000)
;; -Increase max-lisp-eval-depth

;; UnsetPES
;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Doom handles package initialization, so
;; we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)
;; Do not allow loading from the package cache (same reason).
(setq package-quickstart nil)
;; -UnsetPES

;; UnsetFNHA
(setq file-name-handler-alist-original file-name-handler-alist
      file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda() (setq file-name-handler-alist file-name-handler-alist-original)))
;; -UnsetFNHA

;; UnsetSRF
(setq site-run-file nil)
;; -UnsetSRF

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)
;; -Inhibit resizing frame

;; DisableUnnecessaryInterface
(menu-bar-mode -1)
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(scroll-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))
;; -DisableUnnecessaryInterface

;;; early-init.el ends here
