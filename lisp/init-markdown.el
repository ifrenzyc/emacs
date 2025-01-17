;; init-markdown.el --- Initialize markdown settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; Config for setting markdown mode and stuff
;; 
;; 需要先安装 mulitmarkdown
;; macOS:
;; brew install multimarkdown
;; 
;; 参考：
;; - http://aaronbedra.com/emacs.d/
;; - https://leanpub.com/markdown-mode/read#leanpub-auto-quick-reference

;;; Code:
(require 'init-font)

(use-package markdown-mode
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'"       . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :bind
  (:map markdown-mode-map
        ([f12] . markdown-live-preview-mode))
  :hook
  ((markdown-mode . lsp)
   (markdown-mode . (lambda ()
                      (text-scale-increase 1)
                      (setq-local line-spacing 0.5)
                      (setq-local truncate-lines t)
                      (setq-local word-wrap nil))))
  :commands
  (markdown-mode gfm-mode)
  :ensure-system-package (multimarkdown . "brew install multimarkdown")
  :custom-face
  (markdown-table-face ((t (:family "LXGW Bright Code GB" :size 160))))
  ;; (markdown-header-delimiter-face ((t (:foreground "#616161" :height 0.9))))
  ;; (markdown-header-face-1 ((t (:height 1.6  :foreground "#A3BE8C" :weight extra-bold :inherit markdown-header-face))))
  ;; (markdown-header-face-2 ((t (:height 1.4  :foreground "#EBCB8B" :weight extra-bold :inherit markdown-header-face))))
  ;; (markdown-header-face-3 ((t (:height 1.2  :foreground "#D08770" :weight extra-bold :inherit markdown-header-face))))
  ;; (markdown-header-face-4 ((t (:height 1.15 :foreground "#BF616A" :weight bold :inherit markdown-header-face))))
  ;; (markdown-header-face-5 ((t (:height 1.1  :foreground "#b48ead" :weight bold :inherit markdown-header-face))))
  ;; (markdown-header-face-6 ((t (:height 1.05 :foreground "#5e81ac" :weight semi-bold :inherit markdown-header-face))))
  :init
  (setq markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-asymmetric-header t
        markdown-make-gfm-checkboxes-buttons t
        markdown-gfm-uppercase-checkbox t
        markdown-fontify-code-blocks-natively t)

  (setq markdown-command "/opt/homebrew/bin/multimarkdown")

  (setq markdown-content-type "application/xhtml+xml")
  (setq markdown-css-paths '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"
                             "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github.min.css")
        markdown-xhtml-header-content "
<meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>
<style>
body {
  box-sizing: border-box;
  max-width: 740px;
  width: 100%;
  margin: 40px auto;
  padding: 0 10px;
}
</style>

<link rel='stylesheet' href='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/default.min.css'>
<script src='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js'></script>
<script>
document.addEventListener('DOMContentLoaded', () => {
  document.body.classList.add('markdown-body');
  document.querySelectorAll('pre code').forEach((code) => {
    if (code.className != 'mermaid') {
      hljs.highlightBlock(code);
    }
  });
});
</script>

<script src='https://unpkg.com/mermaid@8.4.8/dist/mermaid.min.js'></script>
<script>
mermaid.initialize({
  theme: 'default',  // default, forest, dark, neutral
  startOnLoad: true
});
</script>
")

  ;; Use `which-key' instead
  (advice-add #'markdown--command-map-prompt :override #'ignore)
  (advice-add #'markdown--style-map-prompt   :override #'ignore)
  :hook
  (markdown-mode . (lambda ()
                     (setq-local line-spacing 0.2)
                     (setq-local truncate-lines t)
                     (setq-local word-wrap nil)
                     ))
  :config
  (defun yc/markdown-live-preview-window-xwidget-webkit (file)
    "Preview FILE with xwidget-webkit.
To be used with `markdown-live-preview-window-function'."
    (let ((uri (format "file://%s" file)))
      (xwidget-webkit-browse-url uri)
      xwidget-webkit-last-session-buffer))
  (setq markdown-live-preview-window-function 'yc/markdown-live-preview-window-xwidget-webkit)
  :mode-hydra
  (markdown-mode
   (:title "Markdown Commands")
   ("Format"
    (("h" markdown-insert-header-dwim "header")
     ("1" markdown-insert-header-atx-1 "h1")
     ("2" markdown-insert-header-atx-2 "h2")
     ("3" markdown-insert-header-atx-3 "h3")
     ("4" markdown-insert-header-atx-4 "h4")
     ("s" markdown-insert-bold "bold")
     ("e" markdown-insert-italic "italic")
     ("b" markdown-insert-blockquote "quote")
     ("p" markdown-insert-pre "pre")
     ("c" markdown-insert-code "code"))
    "Other"
    (("l" markdown-promote "promote")
     ("r" markdown-demote "demote")
     ("d" markdown-move-down "move down")
     ("u" markdown-move-up "move up")
     ("L" markdown-insert-link "link")
     ("U" markdown-insert-uri "url")
     ("F" markdown-insert-footnote "footnote")
     ("W" markdown-insert-wiki-link "wiki")
     ("R" markdown-insert-reference-link-dwim "r-link")
     ("n" markdown-cleanup-list-numbers "clean-lists")
     ("C" markdown-complete-bufer "complete"))
    "Table"
    (("k" markdown-table-insert-column "insert column")
     ("K" markdown-table-delete-column "delete column")
     ("i" markdown-table-insert-row "insert row")
     ("I" markdown-table-delete-row "delete row")
     ("M-<left>" markdown-table-move-column-left "move column left")
     ("M-<right>" markdown-table-move-column-right "move column right")
     ("M-<down>" markdown-table-move-row-down "move row down")
     ("M-<up>" markdown-table-move-row-up "move row up"))
    "Pandoc"
    (("A" pandoc-article "article")
     ("B" pandoc-beamer "beamer")
     ("S" pandoc-slides "slides")
     ("H" pandoc-handout "handout")
     ("O" pandoc-obuletter "obu letter")
     ("D" pandoc-docx "docx")
     ("H" pandoc-html "html")
     ("P" pandoc-pdf "pdf")
     ("T" pandoc-clean "trash non-md"))
    "Preview"
    (("p" markdown-preview-mode "toggle preview")
     ("o" markdown-preview-open-browser "open in browser"))
    "Quit"
    (("q" nil "quit")
     ("C-g" nil "quit")))))
;; :preface
;; (defun yc/markdown-set-ongoing-hydra-body ()
;;   (setq yc/ongoing-hydra-body #'hydra-markdown/body))
;; :hook (markdown-mode . yc/markdown-set-ongoing-hydra-body))

(use-package markdown-mode+
  :load-path "localelpa/markdown-mode-plus"
  :after markdown-mode)

(use-package markdown-toc :after markdown-mode)

(use-package markdownfmt
  :after markdown-mode
  :bind
  (:map markdown-mode-map
        ("C-c C-f" . markdownfmt-format-buffer)))

(use-package edit-indirect
  :bind
  (:map edit-indirect-mode-map
        ("C-c C-c" . edit-indirect-commit)
        ("C-c C-k" . edit-indirect-abort)
        ("C-c '"   . nil))
  :hook
  ((edit-indirect-after-creation . aorst/real-buffer-setup)
   (edit-indirect-after-creation . aorst/edit-indirect-header-line-setup))
  :init
  (defun aorst/edit-indirect-header-line-setup ()
    (setq-local
     header-line-format
     (substitute-command-keys
      "\\<edit-indirect-mode-map>Edit, then exit with `\\[edit-indirect-commit]' or abort with `\\[edit-indirect-abort]'"))))

(use-package markdown-preview-mode
  :after markdown-mode
  :config
  (add-to-list 'markdown-preview-javascript "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML")
  (setq markdown-preview-stylesheets
        (list (concat "https://github.com/dmarcotte/github-markdown-preview/"
                      "blob/master/data/css/github.css"))))

;; grip-mode
;; markdown preview
;; macOS:
;; pip install grip
(use-package grip-mode
  :bind
  (:map markdown-mode-command-map
        ("g" . grip-mode))
  :commands (grip-mode)
  :ensure-system-package (grip . "pip install grip")
  :config
  ;; Path to the grip binary
  (setq grip-mode-binary-path (executable-find "grip")))

;;================================================================================
;; (use-package separedit
;;   :hook (separedit-buffer-creation . aorst/separedit-header-line-setup)
;;   :bind (:map prog-mode-map
;;          ("C-c '" . separedit)
;;          :map edit-indirect-mode-map
;;          ("C-c '" . separedit))
;;   :custom
;;   (separedit-default-mode 'markdown-mode)
;;   :init
;;   (defun aorst/separedit-header-line-setup ()
;;     (setq-local
;;      header-line-format
;;      (substitute-command-keys
;;       "Edit, then exit with `\\[separedit-commit]' or abort with `\\[edit-indirect-abort]'"))))
;;================================================================================

(provide 'init-markdown)
;;; init-markdown.el ends here
