;; init-elfeed.el --- Initialize elfeed settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 阅读 RSS
;; - newsticker
;; 

;;; Code
(use-package elfeed
  :commands elfeed
  :custom
  (elfeed-use-curl t)
  (elfeed-curl-max-connections 10)
  (elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory))
  (elfeed-enclosure-default-dir "~/Downloads")
  (elfeed-search-clipboard-type 'CLIPBOARD)
  (elfeed-show-entry-switch #'pop-to-buffer)
  (elfeed-show-entry-delete #'delete-window)
  (elfeed-search-title-max-width (current-fill-column))
  (elfeed-search-title-max-width 100)
  (elfeed-search-title-min-width 30)
  (elfeed-search-trailing-width 16)
  (elfeed-show-truncate-long-urls t)
  (elfeed-show-unique-buffers t)
  :hook
  (elfeed-update . evil-emacs-state)
  :config
  (setq elfeed-curl-extra-arguments '("--proxy" "socks5://127.0.0.1:1080"
                                      "--retry" "3"
                                      "--insecure"))
  (setq elfeed-feeds '(
                       ("http://www.masteringemacs.org/feed/" mastering emacs)
                       ("https://www.reddit.com/r/emacs.rss" reddit emacs)
                       ("http://sachachua.com/blog/category/emacs-news/feed" emacs)
                       ("https://planet.emacslife.com/atom.xml" emacslife emacs)
                       ;; ("https://oremacs.com/atom.xml" oremacs)
                       ;; ("http://pragmaticemacs.com/feed/" pragmaticemacs)
                       ;; ("https://pinecast.com/feed/emacscast" emacscast)
                       ;; ("https://tech.meituan.com/feed" tech)
                       ;; ("http://www.howardism.org/index.xml")     ;; My Blog
                       ;; ("http://planet.emacsen.org/atom.xml")     ;; Emacs RSS
                       ;; ("http://endlessparentheses.com/atom.xml") ;; Emacs Blog
                       ;; ("http://emacs-fu.blogspot.com/feeds/posts/default")
                       ;; ("http://emacsredux.com/atom.xml")         ;; Emacs Blog
                       ;; ("http://www.lunaryorn.com/feed.atom")     ;; Emacs Blog
                       ;; ("http://emacshorrors.com/feed.atom")
                       ;; ("http://swannodette.github.com/atom.xml") ;; David Nolen, duh.
                       ;; ("http://batsov.com/atom.xml")             ;; Bozhidar Batsov
                       ;; ("https://apod.nasa.gov/apod.rss")         ;; Nasa's Picture of the Day
                       ;; ("http://twogreenleaves.org/index.php?feed=rss")
                       ;; ("https://medium.com/feed/@hlship/")       ;; Programming
                       ;; ("http://gigasquidsoftware.com/atom.xml")  ;; Clojure
                       ;; ("http://blog.fogus.me/feed/")      ;; Programming
                       ;; ("http://steve-yegge.blogspot.com/atom.xml")
                       ;; ("http://www.rkn.io/feed.xml")
                       ;; ("http://nullprogram.com/feed/")
                       ;; ("https://coldnew.github.io/rss")
                       ;; ("http://emacsninja.com/emacs.atom" emacs)
                       ;; ("http://emacshorrors.com/feed.atom" emacs)
                       ;; ("http://planet.emacsen.org/atom.xml" emacs)
                       ;; ("https://emacs-china.org/latest.rss" emacs)
                       ;; ("http://ubuntupodcast.org/feed/" ubuntu)
                       ;; ("https://sspai.com/feed")
                       ;; ("http://feeds2.feedburner.com/stevelosh")
                       ;; ("http://feed.williamlong.info/")
                       ;; ("http://matt.might.net/articles/feed.rss")
                       ;; ("https://olivierpieters.be/feed.xml")
                       ;; ("https://two-wrongs.com/feed.xml")
                       ;; ("http://alex-charlton.com/rss.xml")
                       ;; ("https://jameshfisher.com/feed.xml")
                       ;; ("https://open.nytimes.com/feed")
                       ;; ("http://bluxte.net/rss.xml")
                       ;; ("https://increment.com/feed.xml")
                       ))
  :pretty-hydra
  ((:title (pretty-hydra-title "Elfeed" 'faicon "rss-square")
           :color amaranth :quit-key "q")
   ("Search"
    (("c" elfeed-db-compact "compact db")
     ("g" elfeed-search-update--force "refresh")
     ("G" elfeed-search-fetch "update")
     ("y" elfeed-search-yank "copy URL")
     ("+" elfeed-search-tag-all "tag all")
     ("-" elfeed-search-untag-all "untag all"))
    "Filter"
    (("s" elfeed-search-live-filter "live filter")
     ("S" elfeed-search-set-filter "set filter")
     ("*" (elfeed-search-set-filter "@6-months-ago +star") "starred")
     ("A" (elfeed-search-set-filter "@6-months-ago" "all"))
     ("T" (elfeed-search-set-filter "@1-day-ago" "today")))
    "Article"
    (("b" elfeed-search-browse-url "browse")
     ("n" next-line "next")
     ("p" previous-line "previous")
     ("u" elfeed-search-tag-all-unread "mark unread")
     ("r" elfeed-search-untag-all-unread "mark read")
     ("RET" elfeed-search-show-entry "show"))))
  :bind (:map elfeed-search-mode-map
              ("?" . elfeed-hydra/body)
              ("w" . elfeed-search-yank)
              ("g" . elfeed-update)
              ("G" . elfeed-search-update--force)
              :map elfeed-show-mode-map
              ("w" . elfeed-show-yank)))

;; (use-package elfeed-link)

(use-package elfeed-goodies
  :disabled
  :config (elfeed-goodies/setup))

(provide 'init-elfeed)
