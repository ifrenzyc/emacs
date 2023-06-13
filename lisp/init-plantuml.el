;; init-plantuml.el --- Initialize plantuml settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; 
;; 需要依赖 Java 环境及 plantuml.jar（http://plantuml.com/download）。
;; 

;;; Code:

(use-package plantuml-mode
  :init
  (setq plantuml-java-args (expand-file-name "bin/plantuml.jar" user-emacs-directory))
  :config
  ;; Enable plantuml-mode for PlantUML files
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

  ;; Enable plantuml-mode within an org-mode document
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))

(provide 'init-plantuml)
;;; init-plantuml.el ends here
