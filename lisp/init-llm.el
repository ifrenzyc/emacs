;; init-llm.el --- Initialize llm settings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; - https://github.com/s-kostyaev/ellama
;; - https://github.com/karthink/gptel
;; - https://github.com/ahyatt/llm
;;

;;; Code:
(use-package llm
  :if (executable-find "ollama")
  :custom
  (llm-log t)
  :config
  (require 'llm-ollama))

(use-package ellama
  :if (executable-find "ollama")
  :init
  (setopt ellama-enable-keymap t)
  (setopt ellama-keymap-prefix "C-c e")
  (setopt ellama-language "Chinese")
  (setopt ellama-user-nick (car (string-split user-full-name)))
  (require 'llm-ollama)
  
  (defvar +ellama-process-name "ellama-server")
  (defvar +ellama-server-buffer-name " *ellama-server*")
  (defun +ellama-serve ()
    "Start Ellama server."
    (interactive)
    (if (executable-find "ollama")
        (if (get-process +ellama-process-name)
            (message "The Ollama server is already running, call `+ellama-kill-server' to stop it.")
          (if (make-process :name +ellama-process-name :buffer +ellama-server-buffer-name :command '("ollama" "serve"))
              (message "Successfully started Ollama server.")
            (user-error "Cannot start the Ollama server"))
          (with-eval-after-load 'ellama (+ellama-set-providers)))
      (user-error "Cannot find the \"ollama\" executable")))
  (defun +ellama-kill-server ()
    "Kill Ellama server."
    (interactive)
    (let ((ollama (get-process +ellama-process-name)))
      (if ollama
          (if (kill-process ollama)
              (message "Killed Ollama server.")
            (user-error "Cannot kill the Ollama server"))
        (message "No running Ollama server."))))
  :config
  (which-key-add-key-based-replacements "C-c e"   "ellama")
  (which-key-add-key-based-replacements "C-c e a" "Ask")
  (which-key-add-key-based-replacements "C-c e c" "Code")
  (which-key-add-key-based-replacements "C-c e d" "Define")
  (which-key-add-key-based-replacements "C-c e i" "Improve")
  (which-key-add-key-based-replacements "C-c e m" "Make")
  (which-key-add-key-based-replacements "C-c e p" "Provider")
  (which-key-add-key-based-replacements "C-c e s" "Summarize")
  (which-key-add-key-based-replacements "C-c e x" "Context")
  ;; 可以参考这篇内容，查看本地 Ollama 安装了哪些 models 并自动加载
  ;; https://github.com/abougouffa/minemacs/blob/main/modules/me-ai.el#L15
  (defun +ellama-list-installed-models ()
    "Return the installed models"
    (let* ((ret (shell-command-to-string "ollama list"))
           (models (cdr (string-lines ret))))
      (if (and (string-match-p "NAME[[:space:]]*ID[[:space:]]*SIZE[[:space:]]*MODIFIED" ret) (length> models 0))
          (mapcar (lambda (m) (car (string-split m))) models)
        (user-error "Cannot detect installed models, please make sure Ollama server is started"))))
  (defun +ellama-set-providers ()
    (setopt ellama-providers
            (cl-loop for model in (+ellama-list-installed-models)
                     collect (cons model (make-llm-ollama :chat-model model :embedding-model model)))
            ellama-provider (cdr (car ellama-providers))))
  
  (setopt ellama-provider (make-llm-ollama
                           :chat-model "llama3"
                           :embedding-model "llama3:latest"))
  (setopt ellama-naming-provider (make-llm-ollama
                                  :chat-model "qwen:14b"
                                  :embedding-model "qwen:14b"))
  ;; Translation llm provider
  (setopt ellama-translation-provider (make-llm-ollama
                                       :chat-model "mistral"
                                       :embedding-model "mistral:latest"))
  ;; Naming new sessions with llm
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
  
  ;; TODO: 参考这篇代码，配置一份 hydra 方便调用 ellama
  ;; https://github.com/RowPJ/.emacs.d/blob/main/config/ellama-config.el#L79

  ;; my/switch-ollama-provider 函数代码来源 :: https://github.com/erickgnavar/dotfiles/blob/master/.emacs.d/bootstrap.org#ia-models-integration
  (defun my/switch-ollama-provider ()
    "Switch ollama provider by using the installed local models."
    (interactive)
    (let* ((raw-result (shell-command-to-string "ollama list | awk '{print $1}' | tail -n+2"))
           (choices (string-split (string-trim raw-result) "\n"))
           (choices (mapcar (lambda (choice) (car (string-split choice ":"))) choices))
           (model (completing-read "Choose model" choices)))
      (setopt ellama-provider (make-llm-ollama :host "localhost" :chat-model model))
      (message "Model %s configured as ollama provider." (propertize model 'face '(:foreground "magenta")))))
  ;; 使用 ellama 连接 Gemini
  ;; https://github.com/r0man/.emacs.d/blob/master/init.el.org
  )

;; TODO gptel 配置参考
;; https://github.com/cashpw/dotfiles/blob/main/config/doom/config-personal.sync-conflict-20240311-084959-CRTXFY7.org#L445
(use-package gptel
  :custom
  (gptel-default-mode 'org-mode)
  :config
  (setq gptel-model "llama3:latest"
        gptel-backend (gptel-make-ollama "Ollama"
                        :host "localhost:11434"
                        :stream t
                        :models '("llama3:latest"))))

(use-package copilot
  :disabled t
  ;; zerolfx/copilot.el
  ;; TODO: copilot 配置可以参考这份代码
  ;; https://github.com/christianromney/dotfiles/blob/main/private_dot_config/doom/config.el#L787
  ;; https://github.com/proprietary/dotfiles/blob/main/emacs/init.el#L583
  :hook
  (prog-mode  . copilot-mode)
  :bind
  (:map copilot-completion-map
        ("<tab>"             . 'copilot-accept-completion)
        ("TAB"               . 'copilot-accept-completion)
        ("M-TAB"             . 'copilot-accept-completion-by-word)
        ("M-<tab>"           . 'copilot-accept-completion-by-word))
  :custom
  ;; wait two seconds before suggesting
  (copilot-idle-delay 2)
  :config
  ;; https://code.visualstudio.com/docs/languages/identifiers#_known-language-identifiers
  (dolist (item '(("python-ts"     . "python")
                  ("rust-ts"       . "rust")
                  ("java-ts"       . "java")
                  ("c++-ts"        . "cpp")
                  ("c-ts"          . "c")
                  ("go-ts"         . "go")
                  ("javascript-ts" . "javascript")
                  ("typescript-ts" . "typescript")
                  ("ruby-ts"       . "ruby")
                  ("swift-ts"      . "swift")
                  ("yaml-ts"       . "yaml")
                  ("julia-ts"      . "julia")))
    (add-to-list 'copilot-major-mode-alist item)))

(provide 'init-llm)
;;; init-llm.el ends here
