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
  :config
  ;; TODO: 可以参考这篇内容，查看本地 Ollama 安装了哪些 models 并自动加载
  ;; https://github.com/abougouffa/minemacs/blob/557f0a74610d702962fed376bdc0d05be693650a/modules/me-ai.el#L15
  (setq llama3-prv (make-llm-ollama
                    :chat-model "llama3"
                    :embedding-model "llama3:latest"))
  (setq llama2-chinese-prv (make-llm-ollama
                            :chat-model "llama2-chinese"
                            :embedding-model "llama2-chinese:latest"))
  (setq phi3-prv (make-llm-ollama
                  :chat-model "phi3"
                  :embedding-model "phi3:latest"))
  (setq codellama-prv (make-llm-ollama
                       :chat-model "codellama"
                       :embedding-model "codellama:latest"))
  (setq gemma-prv (make-llm-ollama
                   :chat-model "gemma"
                   :embedding-model "gemma:latest"))
  (setopt ellama-provider llama3-prv)
  (setopt ellama-naming-provider phi3-prv)
  (setopt ellama-providers
          '(("phi3"            . phi3-prv)
            ("codellama"       . codellama-prv)
            ("llama3"          . llama3-prv)
            ("llama2-chinese"  . llama2-chinese-prv)
            ("gemma"           . gemma-prv)))
  ;; Naming new sessions with llm
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
  ;; Translation llm provider
  (setopt ellama-translation-provider (make-llm-ollama
                                       :chat-model "mistral"
                                       :embedding-model "mistral:latest"))
  ;; TODO: 参考这篇代码，配置一份 hydra 方便调用 ellama
  ;; https://github.com/RowPJ/.emacs.d/blob/11b5c331daf27c7d02120fab2104e463f0b59e32/config/ellama-config.el#L79

  ;; 代码来源 :: https://github.com/erickgnavar/dotfiles/blob/master/.emacs.d/bootstrap.org#ia-models-integration
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
  ;; https://github.com/proprietary/dotfiles/blob/27d2cd792b679dcda98004ec543adec02f859229/emacs/init.el#L583
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
