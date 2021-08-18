;;; mana.el --- Insert MTG icons                     -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: July 29, 2017
;; Homepage: https://github.com/angrybacon/mdi
;; Keywords: convenience, faces

;; This program is free software. You can redistribute it and/or modify it under
;; the terms of the Do What The Fuck You Want To Public License, version 2 as
;; published by Sam Hocevar.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.
;;
;; You should have received a copy of the Do What The Fuck You Want To Public
;; License along with this program. If not, see http://www.wtfpl.net/.

;;; Commentary:

;; This package provides a function to easily insert Magic: the Gathering icons
;; showcased at https://andrewgioia.github.io/Mana/.

;;; Code:


;; NOTE: Should not be needed anymore thanks to `font-lock-ignore', even on
;;       Windows. Except `font-lock+' isn't available on Windows.
(set-fontset-font "fontset-default" '(#xe000 . #xefff) "mana")

(defconst mana-icons-alist
  '(

    ;; Mana
    ("w" . "\xe600")
    ("u" . "\xe601")
    ("b" . "\xe602")
    ("r" . "\xe603")
    ("g" . "\xe604")
    ("0" . "\xe605")
    ("1" . "\xe606")
    ("2" . "\xe607")
    ("3" . "\xe608")
    ("4" . "\xe609")
    ("5" . "\xe60a")
    ("6" . "\xe60b")
    ("7" . "\xe60c")
    ("8" . "\xe60d")
    ("9" . "\xe60e")
    ("10" . "\xe60f")
    ("11" . "\xe610")
    ("12" . "\xe611")
    ("13" . "\xe612")
    ("14" . "\xe613")
    ("15" . "\xe614")
    ("16" . "\xe62a")
    ("17" . "\xe62b")
    ("18" . "\xe62c")
    ("19" . "\xe62d")
    ("20" . "\xe62e")
    ("x" . "\xe615")
    ("y" . "\xe616")
    ("z" . "\xe617")
    ("phyrexian" . "\xe618")
    ("s" . "\xe619")
    ("c" . "\xe904")
    ("energy" . "\xe907")
    ("tap" . "\xe61a")
    ("untap" . "\xe61b")
    ("tap-alt" . "\xe61c")
    ("chaos" . "\xe61d")
    ("1-2" . "\xe902")
    ("infinity" . "\xe903")

    ;; Card types
    ("artifact" . "\xe61e")
    ("creature" . "\xe61f")
    ("enchantment" . "\xe620")
    ("instant" . "\xe621")
    ("land" . "\xe622")
    ("planeswalker" . "\xe623")
    ("sorcery" . "\xe624")

    ;; Un-set symbols
    ("100" . "\xe900")
    ("1000000" . "\xe901")

    ;; Planeswalker
    ("loyalty-down" . "\xe625")
    ("loyalty-start" . "\xe628")
    ("loyalty-up" . "\xe627")
    ("loyalty-zero" . "\xe626")

    ;; Guilds and Clans
    ("clan-abzan" . "\xe916")
    ("clan-atarka" . "\xe91b")
    ("clan-dromoka" . "\xe91c")
    ("clan-jeskai" . "\xe917")
    ("clan-kolaghan" . "\xe91d")
    ("clan-mardu" . "\xe918")
    ("clan-ojutai" . "\xe91e")
    ("clan-silumgar" . "\xe91f")
    ("clan-sultai" . "\xe919")
    ("clan-temur" . "\xe91a")
    ("guild-azorius" . "\xe90c")
    ("guild-boros" . "\xe90d")
    ("guild-dimir" . "\xe90e")
    ("guild-golgari" . "\xe90f")
    ("guild-gruul" . "\xe910")
    ("guild-izzet" . "\xe911")
    ("guild-orzhov" . "\xe912")
    ("guild-rakdos" . "\xe913")
    ("guild-selesnya" . "\xe914")
    ("guild-simic" . "\xe915")

    ;; Other
    ("dfc-day" . "\xe906")
    ("dfc-emrakul" . "\xe90a")
    ("dfc-ignite" . "\xe908")
    ("dfc-moon" . "\xe90b")
    ("dfc-night" . "\xe905")
    ("dfc-spark" . "\xe909")
    ("flashback" . "\xe629")))


;;;; Variables


(defgroup mana nil
  "Insert MTG icons."
  :group 'appearance
  :prefix "mana-")

(defface mana-face `((t (:family "mana" :height 0.9)))
  "Face for MTG icons."
  :group 'mana)


;;;; Commands


;;;###autoload
(defun mana (icon &optional padded)
  "Return a fontified icon string."
  (let* ((code (cdr (assoc icon mana-icons-alist)))
         (result (propertize code 'face 'mana-face 'rear-nonsticky t)))
    (if padded
        (concat
         (propertize " " 'display '(space . (:width (5))))
         result
         (propertize " " 'display '(space . (:width (5)))))
      result)))


(provide 'mana)
;;; mana.el ends here
