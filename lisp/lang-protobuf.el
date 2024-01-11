;; lang-protobuf.el --- Initialize protobuf settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:
(use-package protobuf-mode
  :mode ("\\.\\(proto\\)$" . protobuf-mode))

(provide 'lang-protobuf)
;;; lang-protobuf.el ends here
