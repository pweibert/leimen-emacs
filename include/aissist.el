;; Define the model registry
(setq ollama-models-registry '())
(setq ollama-llm-providers '())

(require 'llm)
(defgroup aissist nil
  "Large language model code completion."
  :prefix "aissist-"
  :group 'editing)

(defun kill-processes-by-wildcard (pattern)
  "Kill all processes whose name matches the given PATTERN, and suppress save prompts."
  (let ((inhibit-quit t)           ; Disable quitting
        (inhibit-message t)        ; Suppress messages
        (save-silently t))         ; Avoid saving prompts
    (dolist (process (process-list))  ; Loop through all processes
      (let* ((process-name (process-name process))
             (process-buffer (process-buffer process)))  ; Get associated buffer
        (when (string-match pattern process-name)
          (message "Killing buffer: %s" process-buffer)
          (when process-buffer
            ;; killing the hidden buffer terminates the process
            (with-temp-message (or (current-message) "")  ; Suppress buffer messages
              (set-buffer-modified-p nil)  ; Mark buffer as unmodified
              (kill-buffer process-buffer))))))))  ; Kill the process

(defun ensure-ollama-server ()
  (if (string-empty-p (shell-command-to-string "which ollama"))
      (progn (message "error: aissist: please install ollama!")
             (throw 'ollama-server-err nil))
    (unless (= 0 (shell-command "ollama list"))
      (if (y-or-n-p "No ollama server running, start server locally?")
          (progn (start-process "ollama serve" "*ollama serve*" "ollama" "serve") (sleep-for 5)))
      (message "Server start triggered!"))))

(defun keyboard-quit-mod ()
  (interactive)
  (kill-processes-by-wildcard "^plz-request-curl")
  (keyboard-quit))

;; Allow to terminate ai streaming interactively
(global-set-key (kbd "C-g") 'keyboard-quit-mod)

(defun add-ai-gen-markers ()
  (interactive)
  (save-excursion
  (deactivate-mark)
  (newline)
  (insert "AI-GEN-START")
  (comment-line 1)
  (newline 2)
  (previous-line)
  (insert "AI-GEN-END")
  (comment-line 1)
  )
  (next-line 2)
  )

(defun ollama-bin-missing ()
  (if (string-empty-p (shell-command-to-string "which ollama"))
      (progn
        (message "Error: aissist: ollama bin not found. Make sure ollama is installed and available in $PATH")
        (throw 'ollama-bin-missing)
      )
  )
)

(defun determine-language-suffix (file-name)
   (message "current-file: %s" file-name)
   (if (file-name-extension file-name) (file-name-extension curfile) "txt")
   )

(defun aissist-run-ollama-generic (ollama-model)
  "Run llm-chat-streaming-to-point with the aissist-accurate provider at the current point in the current buffer."
  (message "aissist-run-ollama-generic")
  (catch 'ollama-bin-missing
  (catch 'ollama-server-err
  (defvar-local llm-chat-stmreaming-prompt nil)
  (let* ((spot (point))
         (inhibit-quit t)
         (curfile (buffer-name))
         (hist (concat curfile ".prompt"))
         (lang (determine-language-suffix curfile))
         (start (min (region-beginning) (region-end)))
         (end (max (region-beginning) (region-end)))
         (template_prompt "\
language: %s.\n
code:\n%s\n
You are a emacs code generator.\n
Explanations are forbidden!\n
I only want the part to add to my code, not the whole code!\n
I want the result without markdown quotation please!")
         ;; make sure we do not feed more text then is allowed by the maximum context
         (min-start-position (save-excursion (backward-word (- (llm-chat-token-limit ollama-model) (llm-count-tokens ollama-model template_prompt))) (point)))
         (code
          (if (use-region-p)
              ;; Region exists, store boundaries and print them
              (save-excursion
                (buffer-substring-no-properties start end))
            (save-excursion
              (buffer-substring-no-properties min-start-position (point)))
            ))
         (prompt (format template_prompt lang code))
         ) ;; Define a simple prompt
    (ollama-bin-missing)
    (message "aissisxt: prompt: %s" prompt)
    (message "aissist: model provider: \n%s:" ollama-model)
    (ensure-ollama-server)
    (add-ai-gen-markers)
    ;; Call the llm-chat-streaming-to-point with aissist-accurate provider
    ;;(sleep-for 0.1)
    (llm-chat-streaming-to-point
     ollama-model     ;; The provider
     (llm-make-chat-prompt prompt) ;; The prompt for code generation
     (current-buffer)       ;; The current buffer
     (point)        ;; The current point
     (defun dummy-callback () )))))) ;; Dummy callback

(defun build-ollama-llm-providers ()
  "Builds a list of ollama providers.

   The list has elements in this form (provider-name (make-llm-ollama ...)) and thus can be directly passed to ellama.
   "
  (let ((ollama-llm-providers '()))  ;; Locally declared variable with `let`
    (dolist (pair ollama-models-registry)
      (let* ((el_identifier (car pair))
             (ollama_model_reference (cdr pair))
             (provider-name (concat el_identifier "-provider"))
             (provider-instance (make-llm-ollama :embedding-model ollama_model_reference
                                                 :chat-model ollama_model_reference
                                                 :default-chat-temperature 0.9)))
        ;; Modifying the local `ollama-llm-providers` list (created by `let`)
        (setq ollama-llm-providers (add-to-list 'ollama-llm-providers (cons provider-name provider-instance) t))))
    ;; Returning the local variable
    ollama-llm-providers))

(defun register_ollama_llm (el_identifier ollama_model_reference)
  "Registers a llm pair (a b) consisting of a: elisp_model_id b: ollama_model_reference.
Appends the pair to `ollama-models-registry`."
  ;; Append the pair (el_identifier ollama_model_reference) to the list
  (setq ollama-models-registry (append ollama-models-registry (list (cons el_identifier ollama_model_reference))))
  (set (intern el_identifier) ollama_model_reference)
  (message "registering llm: %s" el_identifier))

(defun generate-completion-functions ()
  (dolist (pair ollama-llm-providers)
    (let* ((el_identifier (car pair))
             (ollama_model (cdr pair))
             (function-name (concat "aissist-complete-" (replace-regexp-in-string "-provider" "" (car pair))))
             (global-llm-model-var-name (concat function-name "-llm")))
        ;; Modifying the local `ollama-llm-providers` list (created by `let`)
       (set (intern global-llm-model-var-name) ollama_model)
      ;; (message "defining function: %s, ollama_model: %s, global-llm-model-var-name: %s" function-name ollama_model)
       (eval (read (format "(defun %s () (interactive) (aissist-run-ollama-generic %s))" function-name global-llm-model-var-name)))
    )))

(defun aissist-init ()
  "Initializes package and populates ollama llm provider by setting ollama-llm-providers var."
  (catch 'ollama-bin-missing
    (progn
      (ollama-bin-missing)
      (register_ollama_llm "wizardcoder-33b" "wizardcoder:33b-v1.1-q4_1")
             (setq ollama-llm-providers (build-ollama-llm-providers))
             (generate-completion-functions))))
