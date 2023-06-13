;; lang-logstash.el --- Initialize logstash settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:

;; 修改默认的缩进，原来的是 4 个空格，改成 2 个空格。
(use-package logstash-conf
  :custom
  (logstash-indent 4))

(provide 'lang-logstash)
;;; lang-logstash.el ends here
