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

;;; Code
(require 'init-font)

(use-package markdown-mode
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :commands
  (markdown-mode gfm-mode)
  :ensure-system-package (multimarkdown . "brew install multimarkdown")
  :custom-face
  (markdown-table-face ((t (:family "Sarasa Mono SC" :size user/font-size))))
  :init
  (setq markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-asymmetric-header t
        markdown-make-gfm-checkboxes-buttons t
        markdown-gfm-uppercase-checkbox t
        markdown-fontify-code-blocks-natively t)

  (setq markdown-command "/usr/local/bin/multimarkdown")

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
  ;; Turn on flyspell mode when editing markdown files
  ;; (markdown-mode . flyspell-mode)
  ;; (gfm-mode . flyspell-mode)
  (markdown-mode . (lambda ()
                     ;; (auto-fill-mode t)
                     ;; (set-fill-column 89)
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
  (setq markdown-live-preview-window-function 'yc/markdown-live-preview-window-xwidget-webkit))
;; :preface
;; (defun yc/markdown-set-ongoing-hydra-body ()
;;   (setq yc/ongoing-hydra-body #'hydra-markdown/body))
;; :hook (markdown-mode . yc/markdown-set-ongoing-hydra-body))

(use-package markdown-mode+ :after markdown-mode)
(use-package markdown-toc :after markdown-mode)

(use-package markdownfmt
  :after markdown-mode
  :general
  (markdown-mode-map "C-c C-f" 'markdownfmt-format-buffer))

(use-package markdown-preview-mode
  :after markdown-mode
  :config
  (add-to-list 'markdown-preview-javascript "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML")

  (setq markdown-preview-stylesheets
        (list (concat "https://github.com/dmarcotte/github-markdown-preview/"
                      "blob/master/data/css/github.css"))))

(use-package edit-indirect
  :hook ((edit-indirect-after-creation . aorst/real-buffer-setup)
         (edit-indirect-after-creation . aorst/edit-indirect-header-line-setup))
  :bind (:map
         edit-indirect-mode-map
         ("C-c C-c" . edit-indirect-commit)
         ("C-c C-k" . edit-indirect-abort)
         ("C-c '"   . nil))
  :init
  (defun aorst/edit-indirect-header-line-setup ()
    (setq-local
     header-line-format
     (substitute-command-keys
      "\\<edit-indirect-mode-map>Edit, then exit with `\\[edit-indirect-commit]' or abort with `\\[edit-indirect-abort]'"))))

;; (use-package separedit
;;   :hook (separedit-buffer-creation . aorst/separedit-header-line-setup)
;;   :bind (:map
;;          prog-mode-map
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

;; grip-mode
;; markdown preview
;; macOS:
;; pip install grip
(use-package grip-mode
  :commands (grip-mode)
  ;; :straight (:host github :repo "seagle0128/grip-mode")
  :ensure-system-package (grip . "pip install grip")
  :bind (:map markdown-mode-command-map
              ("g" . grip-mode))
  :config
  ;; Path to the grip binary
  (setq grip-mode-binary-path (executable-find "grip")))

;; (yc/leader-keys-major-mode
;;   :keymaps 'markdown-mode-map
;;   ;; "" '(:ignore t :which-key "major-mode-cmd")
;;   "m." 'hydra-markdown/body)

;; (yc/leader-keys-major-mode-copy
;;   :keymaps 'markdown-mode-map
;;   "" '(:ignore t :which-key "major-mode-cmd")
;;   "m." 'hydra-markdown/body)

;; (defhydra hydra-markdown (:color pink)
;;   "
;; ^
;; ^Markdown^          ^Table Columns^     ^Table Rows^
;; ^────────^──────────^─────────────^─────^──────────^────────
;; _q_ quit            _c_ insert          _r_ insert
;; ^^                  _C_ delete          _R_ delete
;; ^^                  _M-<left>_ left     _M-<down>_ down
;; ^^                  _M-<right>_ right   _M-<up>_ up
;; ^^                  ^^                  ^^
;; "
;;   ("q" nil)
;;   ("c" markdown-table-insert-column)
;;   ("C" markdown-table-delete-column)
;;   ("r" markdown-table-insert-row)
;;   ("R" markdown-table-delete-row)
;;   ("M-<left>" markdown-table-move-column-left)
;;   ("M-<right>" markdown-table-move-column-right)
;;   ("M-<down>" markdown-table-move-row-down)
;;   ("M-<up>" markdown-table-move-row-up))

(major-mode-hydra-bind markdown-mode "Format"
  ("h" markdown-insert-header-dwim "header")
  ("1" markdown-insert-header-atx-1 "h1")
  ("2" markdown-insert-header-atx-2 "h2")
  ("3" markdown-insert-header-atx-3 "h3")
  ("4" markdown-insert-header-atx-4 "h4")
  ("s" markdown-insert-bold "bold")
  ("e" markdown-insert-italic "italic")
  ("b" markdown-insert-blockquote "quote")
  ("p" markdown-insert-pre "pre")
  ("c" markdown-insert-code "code"))
(major-mode-hydra-bind markdown-mode "Other"
  ("l" markdown-promote "promote")
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
(major-mode-hydra-bind markdown-mode "Pandoc"
  ("A" pandoc-article "article")
  ("B" pandoc-beamer "beamer")
  ("S" pandoc-slides "slides")
  ("H" pandoc-handout "handout")
  ("O" pandoc-obuletter "obu letter")
  ("D" pandoc-docx "docx")
  ("H" pandoc-html "html")
  ("P" pandoc-pdf "pdf")
  ("T" pandoc-clean "trash non-md"))
(major-mode-hydra-bind markdown-mode "Preview"
  ("p" markdown-preview-mode "toggle preview")
  ("o" markdown-preview-open-browser "open in browser"))
(major-mode-hydra-bind markdown-mode "Quit"
  ("q" nil "quit")
  ("C-g" nil "quit"))

(provide 'init-markdown)
