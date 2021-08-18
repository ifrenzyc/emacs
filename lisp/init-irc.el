;; init-irc.el --- Initialize irc settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code

(require 'erc)
(require 'erc-log)
(require 'erc-notify)
(require 'erc-spelling)
(require 'erc-autoaway)
(require 'tls)

;; utf-8 always and forever
(setq erc-server-coding-system '(utf-8 . utf-8))
;; Interpret mIRC-style color commands in IRC chats
(setq erc-interpret-mirc-color t)
;; Kill buffers for channels after /part
(setq erc-kill-buffer-on-part t)
;; Kill buffers for private queries after quitting the server
(setq erc-kill-queries-on-quit t)
;; Kill buffers for server messages after quitting the server
(setq erc-kill-server-buffer-on-quit t)
;; exclude boring stuff from tracking
(erc-track-mode t)
(setq erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))

(setq erc-autojoin-channels-alist '(("freenode.net"
                                     "#emacs"
                                     "#python"
                                     "#golang"
                                     "#archlinux-cn"
                                     "#archlinux-offtopic"
                                     "#kubernetes"
                                     )))

(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer (insert-file-contents filePath)
                    (split-string (buffer-string) "\n" t)))

(defun start-irc ()
  (interactive)
  (when (y-or-n-p "Do you want to start IRC? ")
    (let* ((acc (read-lines "~/.irc-account"))
           (irc-nick (car acc))
           (irc-password (nth 1 acc))
           (tls-program '("gnutls-cli --insecure -p %p %h" "gnutls-cli --insecure -p %p %h --protocols ssl3" "openssl s_client -connect %h:%p -no_ssl2 -ign_eof")))
      (erc-tls :server "irc.freenode.net"
               :port 6697
               :nick irc-nick
               :password irc-password))))

(defun filter-server-buffers ()
  (delq nil
        (mapcar
         (lambda (x) (and (erc-server-buffer-p x) x))
         (buffer-list))))

(defun stop-irc ()
  (interactive)
  (dolist (buffer (filter-server-buffers))
    (message "Server buffer: %s" (buffer-name buffer))
    (with-current-buffer buffer
      (erc-quit-server "zzZZ"))))

;; (use-package erc-alert
;;   ;; :disabled t
;;   :load-path "localelpa/erc-alert"
;;   :after erc)

;; ;; (use-package erc-highlight-nicknames
;; ;;   :after erc)

;; (use-package erc-macros
;;   :load-path "localelpa/erc-macros"
;;   :after erc)

;; (use-package erc-patch
;;   :load-path "localelpa/erc-patch"
;;   ;; :disabled t
;;   :after erc)

;; (use-package erc-question
;;   ;; :disabled t
;;   :load-path "localelpa/erc-question"
;;   :after erc)

(use-package erc-yank
  :load-path "localelpa/erc-yank"
  :after erc
  :bind (:map erc-mode-map
              ("C-y" . erc-yank)))

;; (use-package erc
;;   :commands (erc erc-tls)
;;   :bind (:map erc-mode-map
;;               ("C-c r" . reset-erc-track-mode))
;;   :preface
;;   (defun irc (&optional arg)
;;     (interactive "P")
;;     (if arg
;;         (pcase-dolist (`(,server . ,nick)
;;                        '(("irc.freenode.net"     . "ifrenyc")
;;                          ("irc.gitter.im"        . "ifrenyc")
;;                          ;; ("irc.oftc.net"         . "johnw")
;;                          ))

;;           (erc-tls :server server :port 6697 :nick nick
;;                    :password (lookup-password server nick 6697)))

;;       ;;                (let* ((acc (read-lines "~/.irc-account"))
;;       ;;      (irc-nick (car acc))
;;       ;;      (irc-password (nth 1 acc))
;;       ;;      (tls-program '("gnutls-cli --insecure -p %p %h" "gnutls-cli --insecure -p %p %h --protocols ssl3" "openssl s_client -connect %h:%p -no_ssl2 -ign_eof")))
;;       ;; (erc-tls :server "irc.freenode.net"
;;       ;;          :port 6697
;;       ;;          :nick irc-nick
;;       ;;          :password irc-password))

;;       (let ((pass (lookup-password "irc.freenode.net" "ifrenyc" 6697)))
;;         (when (> (length pass) 32)
;;           (error "Failed to read ZNC password"))
;;         (erc :server "127.0.0.1" :port 6697 :nick "ifrenyc"
;;              :password (concat "ifrenyc/gitter:" pass))
;;         (sleep-for 5)
;;         (erc :server "127.0.0.1" :port 6697 :nick "ifrenyc"
;;              :password (concat "ifrenyc/freenode:" pass)))))

;;   (defun reset-erc-track-mode ()
;;     (interactive)
;;     (setq erc-modified-channels-alist nil)
;;     (erc-modified-channels-update)
;;     (erc-modified-channels-display)
;;     (force-mode-line-update))

;;   (defun setup-irc-environment ()
;;     (set (make-local-variable 'scroll-conservatively) 100)
;;     (setq erc-timestamp-only-if-changed-flag nil
;;           erc-timestamp-format "%H:%M "
;;           erc-fill-prefix "          "
;;           erc-fill-column 78
;;           erc-insert-timestamp-function 'erc-insert-timestamp-left
;;           ivy-use-virtual-buffers nil))

;;   (defcustom erc-foolish-content '()
;;     "Regular expressions to identify foolish content.
;;     Usually what happens is that you add the bots to
;;     `erc-ignore-list' and the bot commands to this list."
;;     :group 'erc
;;     :type '(repeat regexp))

;;   (defun erc-foolish-content (msg)
;;     "Check whether MSG is foolish."
;;     (erc-list-match erc-foolish-content msg))

;;   :init
;;   (add-hook 'erc-mode-hook #'setup-irc-environment)
;;   (when alternate-emacs
;;     (add-hook 'emacs-startup-hook 'irc))

;;   (eval-after-load 'erc-identd
;;     '(defun erc-identd-start (&optional port)
;;        "Start an identd server listening to port 8113.
;;   Port 113 (auth) will need to be redirected to port 8113 on your
;;   machine -- using iptables, or a program like redir which can be
;;   run from inetd. The idea is to provide a simple identd server
;;   when you need one, without having to install one globally on
;;   your system."
;;        (interactive (list (read-string "Serve identd requests on port: " "8113")))
;;        (unless port (setq port erc-identd-port))
;;        (when (stringp port)
;;          (setq port (string-to-number port)))
;;        (when erc-identd-process
;;          (delete-process erc-identd-process))
;;        (setq erc-identd-process
;;              (make-network-process :name "identd"
;;                                    :buffer nil
;;                                    :host 'local :service port
;;                                    :server t :noquery t
;;                                    :filter 'erc-identd-filter))
;;        (set-process-query-on-exit-flag erc-identd-process nil)))

;;   :config
;;   (erc-track-minor-mode 1)
;;   (erc-track-mode 1)

;;   (add-hook 'erc-insert-pre-hook
;;             #'(lambda (s)
;;                 (when (erc-foolish-content s)
;;                   (setq erc-insert-this nil)))))

(provide 'init-irc)
