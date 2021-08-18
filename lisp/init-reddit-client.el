;; init-reddit-client.el --- Initialize reddit-client settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; - https://github.com/ahungry/md4rd
;; - https://www.reddit.com/r/emacs/comments/eaf2r0/how_i_use_reddit_from_inside_emacs/
;; 

;;; Code

(use-package md4rd
  :hook
  (md4rd-mode . md4rd-indent-all-the-lines)
  :config
  (setq md4rd-subs-active '(emacs lisp+Common_Lisp prolog clojure))
  (setq md4rd--oauth-access-token
        "your-access-token-here")
  (setq md4rd--oauth-refresh-token
        "your-refresh-token-here")
  (run-with-timer 0 3540 'md4rd-refresh-login))

(provide 'init-reddit-client)
