;; Define the model registry
(setq ollama-models-registry '())
(setq ollama-llm-providers '())

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
                                                 :default-chat-temperature 0.1)))
        ;; Modifying the local `ollama-llm-providers` list (created by `let`)
        (setq ollama-llm-providers (add-to-list 'ollama-llm-providers (cons provider-name provider-instance) t))
        (message "++ build el_identifier %s" el_identifier) ))
    ;; Returning the local variable
    ollama-llm-providers))

(require 'llm)
(defgroup aissist nil
  "Large language model code completion."
  :prefix "aissist-"
  :group 'editing)

(defcustom aissist-fast-model
  "zephyr:latest"
  "Name of the ollama model for faster code completions."
  :type 'string
  :group 'ai-assist)

(defcustom aissist-accurate-model
  ;;  "wizardcoder:33b-v1.1-q4_1"
  "wizardcoder:33b-v1.1-q4_1"
  "Name of the ollama model for accurate code completions."
  :type 'string
  :group 'aissist)

(setq current-llm-request "")

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

(defun keyboard-quit-mod ()
  (interactive)
  (kill-processes-by-wildcard "^plz-request-curl")
  (keyboard-quit))

;; Allow to terminate ai streaming interactively
(global-set-key (kbd "C-g") 'keyboard-quit-mod)


(defun aissist-complete-generic (llm-model)
  "Run the ollama LLM providing predefined prompt and current buffer context for completion."
  (interactive)
  (let* ((spot (point))
         (inhibit-quit t)
         (curfile (buffer-file-name))
         (cash (concat curfile ".cache"))
         (hist (concat curfile ".prompt"))
         (lang (file-name-extension curfile))
         (current-buffer (current-buffer))
         ;; extract current line, to left of caret
         ;; and the previous line, to give the llm
         (code (save-excursion
                 (beginning-of-buffer)
                 (buffer-substring-no-properties (point) spot)))

         ;; create new prompt for this interaction
         (system "\
You are an Emacs code generator. \
Writing comments is forbidden. \
Writing test code is forbidden. \
Writing explanations is forbidden. ")
         (prompt (format
                  "[INST]%sGenerate %s code:[/INST]\n```%s\n%s"
                  (if (file-exists-p cash) "" system) lang lang code)))

    ;; append prompt for current interaction to the big old prompt
    (write-region prompt nil hist nil 'silent)

    ;; call ollama model using `llm-call` with the appropriate model and parameters
    ;; (with-local-quit
    ;;   (apply 'llm- llm-model
    ;;          :input prompt
    ;;          :params (append llm-params (list
    ;;                                      :cache cash
    ;;                                      :silent t
    ;;                                      :max-tokens 2048
    ;;                                      :temperature 0.0
    ;;                                      :n 1))))
    
    (setq current-llm-request (llm-chat-streaming-to-point llm-model prompt current-buffer current-point nil))

    (message ">> post call ")
    ;; get rid of most markdown syntax
    (let ((end (point)))
      (save-excursion
        (goto-char spot)
        (while (search-forward "\\_" end t)
          (backward-char)
          (delete-backward-char 1 nil)
          (setq end (- end 1)))
        (goto-char spot)
        (while (search-forward "```" end t)
          (delete-backward-char 3 nil)
          (setq end (- end 3))))

      ;; append generated code to prompt
      ;;(write-region spot end hist 'append 'silent)
      )))

(defun aissist-dummy-callback (result)
  "Dummy callback that prints the RESULT argument."
  (message "Aissist result: %s" result))

(defun aissist-run-ollama-generic (ollama-model-id)
  "Run llm-chat-streaming-to-point with the aissist-accurate provider at the current point in the current buffer."
  (defvar-local llm-chat-streaming-prompt nil)

  (let* ((spot (point))
         (inhibit-quit t)
         (curfile (buffer-file-name))
         (hist (concat curfile ".prompt"))
         (lang (file-name-extension curfile))

         ;; extract current line, to left of caret
         ;; and the previous line, to give the llm
         (code (save-excursion(beginning-of-buffer)
                 (buffer-substring-no-properties (point) spot)))
         (prompt (format "\

language: %s.\n
code:\n%s\n
You are a emacs code generator.\n
Explanations are forbidden!\n
I only want the part to add to my script, not the whole script!\n
Mark generated code beginning witt 'AI-GEN-START' and end with 'AI-GEN-END' comment." lang code))
        (current-buffer (current-buffer))
        (current-point (point))
        (llm-model-provider (make-llm-ollama :embedding-model ollama-model-id :chat-model ollama-model-id :default-chat-temperature 0.1))
        ) ;; Define a simple prompt

    (message "ai-assistent model: %s" ollama-model-id)
    (message "ai-assistent prompt: \n%s:" prompt)
    ;; Call the llm-chat-streaming-to-point with aissist-accurate provider
    (setq current-llm-request (llm-chat-streaming-to-point
     llm-model-provider     ;; The provider (aissist-accurate)
     (llm-make-chat-prompt prompt) ;; The prompt for code generation
     current-buffer       ;; The current buffer
     current-point        ;; The current point
     'aissist-dummy-callback)))) ;; Dummy callback


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
                                                 :default-chat-temperature 0.1)))
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

(defun aissist-complete-fast ()
  "Run the LLM with faster parameters and insert the completion."
  (interactive)
  (aissist-run-ollama-generic aissist-fast-model))

(defun aissist-complete-accurate ()
  "Run the LLM with accurate parameters and insert the completion."
  (interactive)
  (message "Running aissist accurate model %s" aissist-accurate-model)
  (aissist-run-ollama-generic aissist-accurate-model))

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
;;  (eval (read "(defun mytesttestfunction  () (interactive) (message \"I'm an inner function!\"))"))

  (register_ollama_llm "wizardcoder-33b" "wizardcoder:33b-v1.1-q4_1")
  (setq ollama-llm-providers (build-ollama-llm-providers))
  (generate-completion-functions)

)
