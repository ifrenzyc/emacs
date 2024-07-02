;; init-funcs.el --- Initialize funcs settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 

;;; Code:
(require 'windmove)

(global-set-key (kbd "C-x 2") 'yc/split-window-vertically)
(global-set-key (kbd "C-x 3") 'yc/split-window-horizontally)

(global-set-key (kbd "C-c n n") 'yc/new-buffer-frame)
(global-set-key (kbd "C-x C-m") 'yc/move-file)

;; Split and move the cursor to the new split
(defun yc/split-window-vertically ()
  "Split the window vertically and display the previous buffer."
  (interactive)
  (split-window-vertically)
  (other-window 1)
  (switch-to-next-buffer)
  (switch-to-buffer nil))

(defun yc/split-window-horizontally ()
  "Split the window horizontally and display the previous buffer."
  (interactive)
  (split-window-horizontally)
  (other-window 1)
  (switch-to-next-buffer)
  (switch-to-buffer nil))

(defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-splitter-left-2x ()
  "Move window splitter left 2x speed."
  (interactive)
  (progn
    (hydra-move-splitter-left 2)))

(defun hydra-move-splitter-right-2x ()
  "Move window splitter right 2x speed."
  (interactive)
  (progn
    (hydra-move-splitter-right 2)))

(defun hydra-move-splitter-left-4x ()
  "Move window splitter left 2x speed."
  (interactive)
  (progn
    (hydra-move-splitter-left 4)))

(defun hydra-move-splitter-right-4x ()
  "Move window splitter right 2x speed."
  (interactive)
  (progn
    (hydra-move-splitter-right 4)))

(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

(defun doom/window-enlargen (&optional arg)
  "Enlargen the current window to focus on this one. Does not close other
windows (unlike `doom/window-maximize-buffer'). Activate again to undo."
  (interactive "P")
  (let ((param 'doom--enlargen-last-wconf))
    (cl-destructuring-bind (window . wconf)
        (or (frame-parameter nil param)
            (cons nil nil))
      (set-frame-parameter
       nil param
       (if (and (equal window (selected-window))
                (not arg)
                wconf)
           (ignore
            (let ((source-window (selected-window)))
              (set-window-configuration wconf)
              (when (window-live-p source-window)
                (select-window source-window))))
         (prog1 (cons (selected-window) (or wconf (current-window-configuration)))
           (let* ((window (selected-window))
                  (dedicated-p (window-dedicated-p window))
                  (preserved-p (window-parameter window 'window-preserved-size))
                  (ignore-window-parameters t)
                  (window-resize-pixelwise nil)
                  (frame-resize-pixelwise nil))
             (unwind-protect
                 (progn
                   (when dedicated-p
                     (set-window-dedicated-p window nil))
                   (when preserved-p
                     (set-window-parameter window 'window-preserved-size nil))
                   (maximize-window window))
               (set-window-dedicated-p window dedicated-p)
               (when preserved-p
                 (set-window-parameter window 'window-preserved-size preserved-p))
               (add-hook 'doom-switch-window-hook #'doom--enlargened-forget-last-wconf-h)))))))))

;; steal from - https://github.com/purcell/emacs.d/blob/master/lisp/init-windows.el
;;----------------------------------------------------------------------------
;; Rearrange split windows
;;----------------------------------------------------------------------------
(defun split-window-horizontally-instead ()
  "Kill any other windows and re-split such that the current window is on the top half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-horizontally)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(defun split-window-vertically-instead ()
  "Kill any other windows and re-split such that the current window is on the left half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-vertically)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

;; 通过 =C-c n n= 快速创建一个空的 Buffer。
;; - https://stackoverflow.com/questions/25791605/emacs-how-do-i-create-a-new-empty-buffer-whenever-creating-a-new-frame
;; - http://ergoemacs.org/emacs/emacs_new_empty_buffer.html
(defun yc/new-buffer-frame ()
  "Create a new frame with a new empty buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "Untitled")))
    (set-buffer-major-mode buffer)
    (display-buffer buffer '(display-buffer-pop-up-frame . nil))))

;; 实现 move-file 函数，并映射到 =C-x C-m= 按键上
;; 代码来自这篇文章：http://zck.me/emacs-move-file
(defun yc/move-file (new-location)
  "Write this file to NEW-LOCATION, and delete the old one."
  (interactive (list (expand-file-name
                      (if buffer-file-name
                          (read-file-name "Move file to: ")
                        (read-file-name "Move file to: "
                                        default-directory
                                        (expand-file-name (file-name-nondirectory (buffer-name))
                                                          default-directory))))))
  (when (file-exists-p new-location)
    (delete-file new-location))
  (let ((old-location (expand-file-name (buffer-file-name))))
    (message "old file is %s and new file is %s"
             old-location
             new-location)
    (write-file new-location t)
    (when (and old-location
               (file-exists-p new-location)
               (not (string-equal old-location new-location)))
      (delete-file old-location))))

(defun yc/dired-open-in-finder ()
  "Show current file in OS's file manager."
  (interactive)
  (let ((process-connection-type nil))
    (start-process "" nil "open" ".")))

;; 参考：http://ergoemacs.org/emacs/modernization_mark-word.html
(defun xah-select-text-in-quote ()
  "Select text between the nearest left and right delimiters.
Delimiters here includes the following chars: \"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）
This command does not properly deal with nested brackets.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2015-05-16"
  (interactive)
  (let (-p1
        -p2
        (-skipChars "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）"))
    (skip-chars-backward -skipChars)
    (setq -p1 (point))
    (skip-chars-forward -skipChars)
    (setq -p2 (point))
    (set-mark -p1)))

;; http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(defun endless/narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))
(global-set-key (kbd "C-x n x") 'endless/narrow-or-widen-dwim)

;; macro 的按键绑定可以参考 leuven 函数 leuven-kmacro-turn-on-recording
(defun leuven-kmacro-turn-on-recording ()
  "Start recording a keyboard macro and toggle functionality of key binding."
  (interactive)
  (global-set-key (kbd "<S-f3>") #'leuven-kmacro-turn-off-recording)
  (kmacro-start-macro nil))

(defun leuven-kmacro-turn-off-recording ()
  "Stop recording a keyboard macro and toggle functionality of key binding."
  (interactive)
  (global-set-key (kbd "<S-f3>") #'leuven-kmacro-turn-on-recording)
  (kmacro-end-macro nil))

;; Start/stop recording a keyboard macro.
(global-set-key (kbd "<S-f3>") #'leuven-kmacro-turn-on-recording)

;; Execute the most recent keyboard macro.
(global-set-key (kbd "<f3>") #'kmacro-call-macro)

(provide 'init-funcs)
;;; init-funcs.el ends here
