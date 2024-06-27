;; init-vertico.el --- Initialize vertico settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:

;; Vertico 和 Orderless 主要配置来源： https://kristofferbalintona.me/posts/vertico-marginalia-all-the-icons-completion-and-orderless/#marginalia
(use-package vertico
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  (minibuffer-setup . vertico-repeat-save)
  :bind
  (:map vertico-map
        ("<tab>"     . vertico-insert)    ; Choose selected candidate
        ("<escape>"  . minibuffer-keyboard-quit) ; Close minibuffer
        ;; NOTE 2022-02-05: Cycle through candidate groups
        ("C-M-n"     . vertico-next-group)
        ("C-M-p"     . vertico-previous-group)
        :map minibuffer-local-map
        ("C-c C-o"   . embark-export)
        ("C-c C-c"   . embark-collect)
        :map vertico-map
        ("RET"   . vertico-directory-enter)
        ("DEL"   . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word)
        ("M-P"   . vertico-repeat-previous)
        ("M-N"   . vertico-repeat-next)
        ("S-<prior>"  . vertico-repeat-previous)
        ("S-<next>"   . vertico-repeat-next))
  :custom
  (vertico-count 13)               ; Number of candidates to display
  (vertico-resize nil)
  (vertico-cycle nil)              ; Go from last to first candidate and first to last (cycle)?
  ;; Extensions
  (vertico-grid-separator "       ")
  (vertico-grid-lookahead 50)
  (vertico-buffer-display-action '(display-buffer-reuse-window))
  (vertico-multiform-categories
   '((file reverse)
     (consult-grep buffer)
     (consult-location)
     (imenu buffer)
     (library reverse indexed)
     (org-roam-node reverse indexed)
     (t reverse)))
  (vertico-multiform-commands
   '(("flyspell-correct-*" grid reverse)
     (org-refile grid reverse indexed)
     (consult-yank-pop indexed)
     (consult-flycheck)
     (consult-lsp-diagnostics)))
  :config
  (vertico-mode)
  ;; Prefix the current candidate with “» ”. From
  ;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "» " 'face 'vertico-current)
                   "  ")
                 cand))))

(use-package vertico-prescient
  :init
  (vertico-prescient-mode t))

(use-package marginalia
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle))
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

(use-package nerd-icons-completion
  :after (marginalia-mode)
  :hook
  (marginalia-mode . nerd-icons-completion-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)    ; I want to be in control!
  ;; (completion-category-overrides
  ;;  '((file (styles basic-remote         ; For `tramp' hostname completion with `vertico'
  ;;                  orderless))))
  (orderless-component-separator 'orderless-escapable-split-on-space)
  (orderless-matching-styles
   '(orderless-literal
     orderless-prefixes
     orderless-initialism
     orderless-regexp
     ;; orderless-flex
     ;; orderless-strict-leading-initialism
     ;; orderless-strict-initialism
     ;; orderless-strict-full-initialism
     ;; orderless-without-literal          ; Recommended for dispatches instead
     ))
  (orderless-style-dispatchers
   '(prot-orderless-literal-dispatcher
     prot-orderless-strict-initialism-dispatcher
     prot-orderless-flex-dispatcher))
  :init
  (vertico-mode) ; optional but recommended!
  (defun orderless--strict-*-initialism (component &optional anchored)
    "Match a COMPONENT as a strict initialism, optionally ANCHORED.
The characters in COMPONENT must occur in the candidate in that
order at the beginning of subsequent words comprised of letters.
Only non-letters can be in between the words that start with the
initials.

If ANCHORED is `start' require that the first initial appear in
the first word of the candidate.  If ANCHORED is `both' require
that the first and last initials appear in the first and last
words of the candidate, respectively."
    (orderless--separated-by
        '(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)))
      (cl-loop for char across component collect `(seq word-start ,char))
      (when anchored '(seq (group buffer-start) (zero-or-more (not alpha))))
      (when (eq anchored 'both)
        '(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)) eol))))

  (defun orderless-strict-initialism (component)
    "Match a COMPONENT as a strict initialism.
This means the characters in COMPONENT must occur in the
candidate in that order at the beginning of subsequent words
comprised of letters.  Only non-letters can be in between the
words that start with the initials."
    (orderless--strict-*-initialism component))

  (defun prot-orderless-literal-dispatcher (pattern _index _total)
    "Literal style dispatcher using the equals sign as a suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 0 -1))))

  (defun prot-orderless-strict-initialism-dispatcher (pattern _index _total)
    "Leading initialism  dispatcher using the comma suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "," pattern)
      `(orderless-strict-initialism . ,(substring pattern 0 -1))))

  (defun prot-orderless-flex-dispatcher (pattern _index _total)
    "Flex  dispatcher using the tilde suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "." pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))
  )

(use-package consult
  :bind
  (([remap imenu]              . consult-imenu)
   ([remap switch-to-buffer]   . consult-buffer)
   ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
   ([remap goto-line]          . consult-goto-line)
   ([remap bookmark-jump]      . consult-bookmark)
   ([remap recentf-open-files] . consult-recent-file))
  :config
  (with-no-warnings
    (consult-customize consult-ripgrep consult-git-grep consult-grep
                       consult-bookmark
                       consult-recent-file
                       consult-buffer
                       :preview-key nil))
  :custom
  (consult-fontify-preserve nil)
  (consult-async-min-input 2)
  (consult-async-refresh-delay 0.15)
  (consult-async-input-throttle 0.2)
  (consult-async-input-debounce 0.1)
  (consult-project-root-function #'projectile-project-root))

(use-package consult-git-log-grep
  :custom
  (consult-git-log-grep-open-function #'magit-show-commit))

(use-package consult-dir)
(use-package consult-flycheck)
(use-package consult-lsp)
(use-package consult-org-roam)
(use-package consult-projectile)

(use-package all-the-icons-completion
  :disabled t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(provide 'init-vertico)
;;; init-vertico.el ends here
