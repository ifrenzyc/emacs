;; init-diff.el --- Initialize diff configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:

;; 同时，它也可以应用在 ediff 上，恢复由 ediff 导致的窗体变动。
;; https://github.com/justbur/emacs-vdiff
(use-package ediff
  :ensure nil
  :hook (;; show org ediffs unfolded
         (ediff-prepare-buffer . outline-show-all)
         ;; restore window layout when done
         (ediff-quit . winner-undo))
  :custom
  (ediff-diff-options "-w")
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-merge-split-window-function 'split-window-horizontally))

;; (use-package evil-ediff)

;; Resolve diff3 conflicts
(use-package smerge-mode
  :ensure nil
  :diminish
  :pretty-hydra
  ((:title (pretty-hydra-title "Smerge" 'octicon "diff")
           :color pink :quit-key "C-g")
   ("Move"
    (("n" smerge-next "next")
     ("p" smerge-prev "previous"))
    "Keep"
    (("b" smerge-keep-base "base")
     ("u" smerge-keep-upper "upper")
     ("l" smerge-keep-lower "lower")
     ("a" smerge-keep-all "all")
     ("RET" smerge-keep-current "current")
     ("C-m" smerge-keep-current "current"))
    "Diff"
    (("<" smerge-diff-base-upper "upper/base")
     ("=" smerge-diff-upper-lower "upper/lower")
     (">" smerge-diff-base-lower "upper/lower")
     ("R" smerge-refine "refine")
     ("E" smerge-ediff "ediff"))
    "Other"
    (("C" smerge-combine-with-next "combine")
     ("r" smerge-resolve "resolve")
     ("k" smerge-kill-current "kill")
     ("ZZ" (lambda ()
             (interactive)
             (save-buffer)
             (bury-buffer))
      "Save and bury buffer" :exit t))))
  :hook
  ((find-file . (lambda ()
                  (save-excursion
                    (goto-char (point-min))
                    (when (re-search-forward "^<<<<<<< " nil t)
                      (smerge-mode 1)))))
   (magit-diff-visit-file . (lambda ()
                              (when smerge-mode
                                (hydra-smerge/body))))))

(provide 'init-diff)
;;; init-diff.el ends here
