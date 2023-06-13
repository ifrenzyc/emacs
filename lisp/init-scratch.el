;; init-scratch.el --- Initialize scratch configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:

(setq initial-scratch-message "\
;; ╔═╗┌─┐┬─┐┌─┐┌┬┐┌─┐┬ ┬
;; ╚═╗│  ├┬┘├─┤ │ │  ├─┤
;; ╚═╝└─┘┴└─┴ ┴ ┴ └─┘┴ ┴
;; ═════════════════════


")

;; Recreate scratch buffer
;; just recreates scratch buffer when it's killed, I got this from the EmacsWiki
(defun kill-scratch-buffer ()
  ;; The next line is just in case someone calls this manually
  (set-buffer (get-buffer-create "*scratch*"))
  ;; Kill the current (*scratch*) buffer
  (remove-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  (kill-buffer (current-buffer))
  ;; Make a brand new *scratch* buffer
  (set-buffer (get-buffer-create "*scratch*"))
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  ;; Since we killed it, don't let caller do that.
  nil)

(with-current-buffer "*scratch*"
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer))

;; Persistent the scratch buffer
(use-package persistent-scratch
  :hook ((after-init . persistent-scratch-autosave-mode)
         (lisp-interaction-mode . persistent-scratch-mode)))

(provide 'init-scratch)
;;; init-scratch.el ends here
