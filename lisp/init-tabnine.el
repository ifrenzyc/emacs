;; init-tabnine.el --- Initialize tabnine settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; - https://github.com/TommyX12/company-tabnine
;; - https://tabnine.com/
;; - https://emacs-china.org/t/tabnine/9988/50
;; - Kite: https://jackpopc.github.io/2019/11/16/tabnine/
;; 
;; M-x company-tabnine-install-binary
;; 

;;; Code
(use-package company-tabnine
  :after (company)
  :config
  (with-eval-after-load 'company
    (add-to-list 'company-backends '(company-tabnine company-dabbrev))
    ;; `:separate`  使得不同 backend 分开排序
    (add-to-list 'company-backends '(company-lsp :with company-tabnine :separate))

    ;; 可以通过 company-transformers 实现 TabNine 和 lsp 同时用
    (defun company//sort-by-tabnine (candidates)
      (if (or (functionp company-backend)
              (not (and (listp company-backend) (memq 'company-tabnine company-backend))))
          candidates
        (let ((candidates-table (make-hash-table :test #'equal))
              candidates-1
              candidates-2)
          (dolist (candidate candidates)
            (if (eq (get-text-property 0 'company-backend candidate)
                    'company-tabnine)
                (unless (gethash candidate candidates-table)
                  (push candidate candidates-2))
              (push candidate candidates-1)
              (puthash candidate t candidates-table)))
          (setq candidates-1 (nreverse candidates-1))
          (setq candidates-2 (nreverse candidates-2))
          (nconc (seq-take candidates-1 2)
                 (seq-take candidates-2 2)
                 (seq-drop candidates-1 2)
                 (seq-drop candidates-2 2)))))

    (add-to-list 'company-transformers 'company//sort-by-tabnine t))

  ;; The free version of TabNine is good enough,
  ;; and below code is recommended that TabNine not always
  ;; prompt me to purchase a paid version in a large project.
  ;; (defadvice company-echo-show (around disable-tabnine-upgrade-message activate)
  ;;   (let ((company-message-func (ad-get-arg 0)))
  ;;     (when (and company-message-func
  ;;                (stringp (funcall company-message-func)))
  ;;       (unless (string-match "The free version of TabNine only indexes up to" (funcall company-message-func))
  ;;         ad-do-it))))
  )

(provide 'init-tabnine)
