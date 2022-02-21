;; init-svg-tag.el --- Initialize svg-tag-mode configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; - https://github.com/rougier/svg-tag-mode
;; - https://github.com/rougier/svg-tag-mode/blob/main/examples/example-2.el
;; - https://github.com/shaunsingh/nyoom.emacs/blob/d6cd50faec13a4050c3898c27147c5768fa832f9/config.org
;; - https://github.com/wowhxj/.emacs.d/blob/59019a364df8129ab9e0f9c0744401444ca09ee4/emacs-config-straight.org
;; - https://github.com/lasvice/dotemacs/blob/cf30155f97e86bc506bdb1b0e3b538552e075bd1/init.el
;; - https://github.com/ahyatt/emacs-setup/blob/bfe643f1533d908bad1d4ff315b2a96ae672ce3d/.emacs.d/emacs.org
;; - https://github.com/jamesnvc/dotfiles/blob/fa89358efec892c27471e4d7c12e72bf8d09b73c/emacs.d/modules/cogent-appearance.el
;; - https://github.com/ody55eus/dotfiles/blob/d78790d0af140b316e415c7d90c1f365211bbd64/doom/Emacs.org
;; - https://github.com/lasvice/dotemacs/blob/cf30155f97e86bc506bdb1b0e3b538552e075bd1/init.el
;; - https://github.com/quantumish/.emacs.d/blob/fa7b4eaeae95a00db2743c20daa92d082e1dc7bd/lisp/tagconfig.el
;;

;;; Code

(use-package svg-tag-mode
  :straight (:host github :repo "rougier/svg-tag-mode" :branch "main")
  :hook (org-mode . svg-tag-mode)
  :config
  (defface svg-tag-todo-face
    '((t :foreground "red" :background "green"
       :family "Hack" :weight normal :height 115))
    "Face for todo note" :group nil)
  ;; Adapted from https://github.com/rougier/svg-tag-mode/blob/main/examples/example-2.el
  (let ((date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
        (time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
        (day-re "[A-Za-z]\\{3\\}"))
    (setq-default svg-tag-tags
                  `(
                    ;; Org tags
                    (":\\([A-Za-z0-9@]+\\)" . ((lambda (tag) (svg-tag-make tag))))
                    (":\\([A-Za-z0-9@]+[ \-]\\)" . ((lambda (tag) tag)))

                    ;; Task priority
                    ("\\[#[A-Z]\\]" . ( (lambda (tag)
                                          (svg-tag-make tag :face 'org-priority 
                                                        :beg 2 :end -1 :margin 0))))

                    ;; TODO states
                    ("TODO|EXTREVIEW|SUBMIT|WAITING" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0))))
                    ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))


                    ;; Citation of the form [cite:@Knuth:1984] 
                    ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
                                                      (svg-tag-make tag
                                                                    :inverse t
                                                                    :beg 7 :end -1
                                                                    :crop-right t))))
                    ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
                                                               (svg-tag-make tag
                                                                             :end -1
                                                                             :crop-left t))))

                    ;; Active date (without day name, with or without time)
                    (,(format "\\(<%s>\\)" date-re) .
                      ((lambda (tag)
                         (svg-tag-make tag :beg 1 :end -1 :margin 0))))
                    (,(format "\\(<%s *\\)%s>" date-re time-re) .
                      ((lambda (tag)
                         (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
                    (,(format "<%s *\\(%s>\\)" date-re time-re) .
                      ((lambda (tag)
                         (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

                    ;; Inactive date  (without day name, with or without time)
                    (,(format "\\(\\[%s\\]\\)" date-re) .
                      ((lambda (tag)
                         (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
                    (,(format "\\(\\[%s *\\)%s\\]" date-re time-re) .
                      ((lambda (tag)
                         (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
                    (,(format "\\[%s *\\(%s\\]\\)" date-re time-re) .
                      ((lambda (tag)
                         (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))
                    )))
  )

(provide 'init-svg-tag)
