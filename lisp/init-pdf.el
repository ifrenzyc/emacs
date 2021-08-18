;; init-pdf.el --- Initialize pdf settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 在 Emacs 中查看 pdf
;; 参考：https://irreal.org/blog/?p=7000
;; 在 org 上打开 pdf 例子
;; [[pdfview:~/Desktop/Ansible 介绍.pdf]]
;; 

;;; Code
(use-package pdf-tools
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page
                pdf-view-use-imagemagick t
                pdf-view-use-scaling t
                pdf-view-midnight-colors '("white smoke" . "gray5"))
  :general
  (pdf-view-mode-map
   "\\" 'hydra-pdftools/body
   "<s-spc>" 'pdf-view-scroll-down-or-next-page
   "g"  'pdf-view-first-page
   "G"  'pdf-view-last-page
   "l"  'image-forward-hscroll
   "h"  'image-backward-hscroll
   "j"  'pdf-view-next-page
   "k"  'pdf-view-previous-page
   "e"  'pdf-view-goto-page
   "u"  'pdf-view-revert-buffer
   "al" 'pdf-annot-list-annotations
   "ad" 'pdf-annot-delete
   "aa" 'pdf-annot-attachment-dired
   "am" 'pdf-annot-add-markup-annotation
   "at" 'pdf-annot-add-text-annotation
   "y"  'pdf-view-kill-ring-save
   "i"  'pdf-misc-display-metadata
   "s"  'pdf-occur
   "b"  'pdf-view-set-slice-from-bounding-box
   "r"  'pdf-view-reset-slice))

(use-package pdf-view
  :straight (:type built-in)
  :after pdf-tools
  :custom
  (pdf-view-resize-factor 1.1)
  (pdf-view-use-unicode-ligther nil))

(use-package org-pdfview
  :after org)

(use-package org-pdftools
  :init (setq ;; org-pdftools-root-dir /path/you/store/pdfs
         org-pdftools-search-string-separator "??")
  :after org
  :config
  (org-link-set-parameters "pdftools"
                           :follow #'org-pdftools-open
                           :complete #'org-pdftools-complete-link
                           :store #'org-pdftools-store-link
                           :export #'org-pdftools-export)
  (add-hook 'org-store-link-functions 'org-pdftools-store-link))

;; (use-package org-noter-pdftools
;;   :after org-noter))

;; Emacs has the built-in DocView mode which lets you view PDFs. The
;; below setting allows continue scrolling
(setq doc-view-continuous t)

;; (bind-keys :map pdf-view-mode-map
;;            ("\\" . hydra-pdftools/body))
(defhydra hydra-pdftools (:color red :hint nil)
  "
                                                                          ╭───────────┐
           Move  History   Scale/Fit     Annotations  Search/Link    Do   │ PDF Tools │
       ╭──────────────────────────────────────────────────────────────────┴───────────╯
             ^^_g_^^      _B_    ^↧^    _+_    ^ ^     [_al_] list    [_s_] search    [_u_] revert buffer
             ^^^↑^^^      ^↑^    _H_    ^↑^  ↦ _W_ ↤   [_am_] markup  [_o_] outline   [_i_] info
             ^^_p_^^      ^ ^    ^↥^    _0_    ^ ^     [_at_] text    [_F_] link      [_d_] dark mode
             ^^^↑^^^      ^↓^  ╭─^─^─┐  ^↓^  ╭─^ ^─┐   [_ad_] delete  [_f_] search link
        _h_ ←pag_e_→ _l_  _N_  │ _P_ │  _-_    _b_     [_aa_] dired
             ^^^↓^^^      ^ ^  ╰─^─^─╯  ^ ^  ╰─^ ^─╯   [_y_]  yank
             ^^_n_^^      ^ ^  _r_eset slice box
             ^^^↓^^^
             ^^_G_^^
       --------------------------------------------------------------------------------
            "
  ("\\" hydra-master/body "back")
  ("<ESC>" nil "quit")
  ("al" pdf-annot-list-annotations)
  ("ad" pdf-annot-delete)
  ("aa" pdf-annot-attachment-dired)
  ("am" pdf-annot-add-markup-annotation)
  ("at" pdf-annot-add-text-annotation)
  ("y" pdf-view-kill-ring-save)
  ("+" pdf-view-enlarge :color red)
  ("-" pdf-view-shrink :color red)
  ("0" pdf-view-scale-reset)
  ("H" pdf-view-fit-height-to-window)
  ("W" pdf-view-fit-width-to-window)
  ("P" pdf-view-fit-page-to-window)
  ("n" pdf-view-next-page-command :color red)
  ("p" pdf-view-previous-page-command :color red)
  ("d" pdf-view-dark-minor-mode)
  ("b" pdf-view-set-slice-from-bounding-box)
  ("r" pdf-view-reset-slice)
  ("g" pdf-view-first-page)
  ("G" pdf-view-last-page)
  ("e" pdf-view-goto-page)
  ("o" pdf-outline)
  ("s" pdf-occur)
  ("i" pdf-misc-display-metadata)
  ("u" pdf-view-revert-buffer)
  ("F" pdf-links-action-perfom)
  ("f" pdf-links-isearch-link)
  ("B" pdf-history-backward :color red)
  ("N" pdf-history-forward :color red)
  ("l" image-forward-hscroll :color red)
  ("h" image-backward-hscroll :color red))

;; 这里的内容是采用 Emacs+orgmode+LaTeX 导出 pdf 相关的。
;; 需要系统安装 xelatex 用于导出带有中文的 pdf。
;; 1. 需要先安装 MacTeX
;; 添加对 xelatex 的支持。xelatex 在刚才安装的 MacTeX 已经安装了。
(require 'ox-beamer)
(require 'ox-latex)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/texlive/2020/bin/x86_64-darwin/"))
(setq exec-path (append exec-path '("/usr/local/texlive/2020/bin/x86_64-darwin/")))

;; 配置使用 xelate 输出中文 pdf
;; org-mode 默认调用的是 pdflatex, 因此需要重新设置编译引擎为 xelatex
;; Use XeLaTeX to export PDF in Org-mode
(setq org-latex-pdf-process '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                              "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                              "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
;; 设置默认后端为 `xelatex'
(setq org-latex-compiler "xelatex")

;; 要导出到 PDF 时也高亮, 需要在 .emacs 文件中加入以下代码:
;; use minted to highlight code in latex
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)
;; add scrartcl LaTeX class to org
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("scrartcl"
                 "\\documentclass{scrartcl}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '(("article"
                  "
  \\documentclass[12pt,a4paper]{article}
  \\usepackage[margin=2cm]{geometry}
  \\usepackage{fontspec}
  \\setromanfont{STSong}
  \\usepackage{etoolbox}  % Quote 部份的字型設定
  \\newfontfamily\\quotefont{STSong}
  \\AtBeginEnvironment{quote}{\\quotefont\\small}
  \\setmonofont[Scale=0.9]{Courier} % 等寬字型 [FIXME] Courier 中文會爛掉！
  \\font\\cwSong=''STSong'' at 10pt
  %\\font\\cwHei=''STSong'' at 10p %不知為何這套字型一用就爆掉...
  \\font\\cwYen=''STSong'' at 10pt
  \\font\\cwKai=''STSong'' at 10pt
  \\font\\cwMing=''STSong'' at 10pt
  \\font\\wqyHei=''STSong'' at 10pt
  \\font\\wqyHeiMono=''STSong'' at 10pt
  \\font\\wqyHeiMicro=''STSong'' at 10pt
  \\XeTeXlinebreaklocale ``zh''
  \\XeTeXlinebreakskip = 0pt plus 1pt
  \\linespread{1.36}
  % [FIXME] ox-latex 的設計不良導致 hypersetup 必須在這裡插入
  \\usepackage{hyperref}
  \\hypersetup{
    colorlinks=true, %把紅框框移掉改用字體顏色不同來顯示連結
    linkcolor=[rgb]{0,0.37,0.53},
    citecolor=[rgb]{0,0.47,0.68},
    filecolor=[rgb]{0,0.37,0.53},
    urlcolor=[rgb]{0,0.37,0.53},
    pagebackref=true,
    linktoc=all,}
  "
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                 ))
  ;; [FIXME]
  ;; 原本是不要讓 org 插入 hypersetup（因為 org-mode 這部份設計成沒辦法自訂，或許可以去 report 一下？）
  ;; 改成自行插入，但這樣 pdfcreator 沒辦法根據 Emacs 版本插入，pdfkeyword 也會無效...幹。
  (setq org-latex-with-hyperref t)
  ;; 把預設的 fontenc 拿掉
  ;; 經過測試 XeLaTeX 輸出 PDF 時有 fontenc[T1]的話中文會無法顯示。
  ;; hyperref 也拿掉，改從 classes 處就插入，原因見上面 org-latex-with-hyperref 的說明。
  (setq org-latex-default-packages-alist
        '(("" "hyperref" nil)
          ("AUTO" "inputenc" t)
          ("" "fixltx2e" nil)
          ("" "graphicx" t)
          ("" "longtable" nil)
          ("" "float" nil)
          ("" "wrapfig" nil)
          ("" "rotating" nil)
          ("normalem" "ulem" t)
          ("" "amsmath" t)
          ("" "textcomp" t)
          ("" "marvosym" t)
          ("" "wasysym" t)
          ("" "multicol" t)  ; 這是我另外加的，因為常需要多欄位文件版面。
          ("" "amssymb" t)
          "\\tolerance=1000"))
  ;; 指定你要用什麼外部 app 來開 pdf 之類的檔案。我是偷懶所以直接用 kde-open，你也可以指定其他的。
  (setq org-file-apps '((auto-mode . emacs)
                        ("\\.mm\\'" . default)
                        ;; ("\\.x?html?\\'" . "xdg-open %s")
                        ;; ("\\.pdf\\'" . "kde-open %s")
                        ;; ("\\.jpg\\'" . "kde-open %s")
                        )))

(provide 'init-pdf)
