;; init-bookmark.el --- Initialize bookmark settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:
(use-package bookmark
  :ensure nil
  :custom
  ;; disable bookmark face after org-capture
  (bookmark-fringe-mark nil))

(provide 'init-bookmark)
;;; init-bookmark.el ends here
