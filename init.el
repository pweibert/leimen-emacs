(setq debug-on-error t) ;;show backtrace on elisp error

;; Save backup files to backup directory only -- not working
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))
(setq make-backup-files nil) ; stop creating ~ files

;; Uncomment iOAn case you have signature verification issues (keyring too old)
(setq package-check-signature nil)

;; Make sure emacs finds executables for lsp installed with pip
(add-to-list 'exec-path "~/.local/bin")
(add-to-list 'exec-path "~/.pyenv/bin/pyenv")
(add-to-list 'exec-path "~/.config/nvm/versions/node/v18.16.0/bin/")
;; ===================================

;; MELPA Package Support

;; ===================================

;; Enables basic packaging support
(require 'package)

;; Adds the Melpa archive to the list of available repositories

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; (add-to-list 'package-archives
;;              '("gnu" . "http://elpa.gnu.org/packages/") t)

;; If there are no archived package contets, refresh them
;; Initializes the package infrastructure
(package-initialize)
(package-refresh-contents)
(when (not package-archive-contents)
  (package-refresh-contents))

;; Installs
;; myPackages contains a list of package names
(defvar myPackages
  '(
    gnu-elpa-keyring-update;; Update signature of package registry
    avy
    ace-window
    beacon
    better-defaults;; Set up some better Emacs defaults
    browse-kill-ring
    company
    ;cmake-mode
    dap-mode
    dockerfile-mode
    drag-stuff
    ein ;; ipython notebook integration
    elpy ;; Emacs Lisp Python Environment
    flutter
    flycheck ;; On the fly syntax checking
    helm-lsp
    helm-projectile ;; Look for files in project
    helm-xref
    hydra
    ;;emacs-jupyter
    ;; jupyter ;; Jupyter notebook integration
    js2-mode
    json-mode
    k8s-mode
    kkp
;;    kubernetes
;;    kubernetes-tramp
    leerzeichen
    lsp-jedi
    lsp-mode
    lsp-dart
    lsp-treemacs
;;    lsp-ui
    llm
    magit;; Git integration
    markdown-mode
    cl-lib
    ;;multiple-cursors
    vterm ;; Seems like vterm installation has to precede multi-vterm installation
    multi-vterm
    nlinum
    origami
    projectile
;;    py-autopep8 ;; Code formatting
    python
    ssh
    spacemacs-theme
    tide
    undo-tree
    use-package
    web-mode
    winum
    which-key
    yaml
    yaml-mode
    yasnippet
    ztree))

;; Scans the list in myPackages
;; If the package listed is not already installed, install it
(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      myPackages)


;; Setup straight package manager for github packages
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

 (transient-mark-mode 1)

(use-package multiple-cursors
  :ensure t
  :straight (multiple-cursors :type git :host github :repo "magnars/multiple-cursors.el")
  :init
  (require 'multiple-cursors))

(require 'multiple-cursors)


(require 'origami)
(require 'vterm)
(require 'multi-vterm)

(setq vterm-max-scrollback 50000)

;; Ediff preferences
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-keep-variants nil)
(setq ediff-merge-revisions-with-ancestor t)
(setq ediff-make-buffers-readonly-at-startup nil)

(require 'browse-kill-ring)

;;(load-theme 'spacemacs-light t)      ;; Load theme
(load-theme 'spacemacs-dark t)      ;; Load theme
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-message t)    ;; Hide the startup message

(require 'ein)
(require 'nlinum)
(global-linum-mode)               ;; Enable line numbers globally
(helm-mode)

(require 'drag-stuff)               ;; Moving words and regions
(drag-stuff-define-keys)
(drag-stuff-global-mode)            ;; Enable dragging everywhere

(require 'helm-projectile)
(require 'helm-xref)
(require 'org)
(require 'ssh)
(require 'web-mode)

(use-package clipetty
  :ensure t
  :straight (clipetty :type git :host github :repo "pweibert/clipetty")
  :hook (after-init . global-clipetty-mode))

(use-package kkp
  :ensure t
  :config
  ;; (setq kkp-alt-modifier 'alt) ;; use this if you want to map the Alt keyboard modifier to Alt in Emacs (and not to Meta)
  (global-kkp-mode +1)
  :hook (emacs-startup . kkp-enable-in-terminal)
  )

(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)

(which-key-mode)
(add-hook 'prog-mode-hook #'lsp)
(setq gc-cons-threshold (* 100 1024 1024)
      company-idle-delay 0.2
      company-minimum-prefix-length 1
      create-lockfiles nil) ;; lock files will kill npm start

(with-eval-after-load 'lsp-mode
  (require 'dap-chrome)
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (yas-global-mode))


;; =============================
;; Configure lsp mode for python
;; =============================
;; Make sure to install pip install debugpy
(require 'dap-python)
;; if you installed debugpy, you need to set this
;; https://github.com/emacs-lsp/dap-mode/issues/306
(setq dap-python-debugger 'debugpy)

;; Enabling only some features
(setq dap-auto-configure-features '(sessions locals controls tooltip))

(dap-register-debug-template "Debug Current Buffer"
  (list :type "python"
        :args "--input_file \"/home/usercmr/Downloads/DECIPHER- HFpEF - MASTERLISTE -MDAT.xlsx\" --output_folder \"/home/usercmr/Goethe CVI Dropbox/Paul Weibert/pseudonymization_output/\" -n -f -d"
        :cwd nil
        :env '(("DEBUG" . "1"))
        :program nil
        :request "launch"
        :name "Python Debug Template"))

(require 'flycheck)
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "jsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\Tiltfile\\'" . python-mode))

(require 'tide)
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)


;; ==========
;; Kubernetes
;; ==========
(setq kubernetes-poll-frequency 3600)
(setq kubernetes-redraw-frequency 3600)

;; Enable elpy
;;(elpy-enable)
;; Enable Flycheck
;;(when (require 'flycheck nil t)
;;  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;  (add-hook 'elpy-mode-hook 'flycheck-mode))


;; configure jsx-tide checker to run after your default jsx checker
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)
(setq lsp-disabled-clients `(copilot-ls))
;;(setq-default dired-listing-switches "-alh1")

;; Enable autopep-8
;;(require 'py-autopep8)
(require 'lsp-dart)
(add-hook 'dart-mode-hook 'lsp) ;; Start LSP when dart-mode starts
;;(setq lsp-dart-flutter-sdk-dir "/home/paul/flutter/")

;; Enable visualization of whitespaces
(require 'leerzeichen)

(windmove-default-keybindings) ;; Move cursor between windows

;; Define function to interrupt shell
;;(defun elpy-shell-interrupt ()
;;  "Interrupt the process associated to the current python script."
;;  (interactive)
;;  (elpy-shell-switch-to-shell)
;;  (interrupt-process)
;;  (elpy-shell-switch-to-buffer))

;; Configure company-tabnine
;; (require 'company-tabnine)
;; (add-to-list 'company-backends #'company-tabnine)

(global-set-key [s-p] 'helm-projectile)

;; Match parentheses
(show-paren-mode 1);; User-Defined init.el ends here

;; Make sure files that change on disk are updated in emacs
(global-auto-revert-mode t)

;; Prevent emacs from creating backup files
(setq make-backup-files nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
  '(dap-python-executable "python3")
 '(package-selected-packages
   '(ac-html-bootstrap clipetty kkp which-key web-mode undo-tree tide spacemacs-theme magit lsp-jedi leerzeichen kubernetes k8s-mode json-mode js2-mode helm-xref helm-projectile helm-lsp elpy drag-stuff dockerfile-mode dap-mode better-defaults))
 '(show-trailing-whitespace t)
 '(term-buffer-maximum-size 8192000))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Always show cursor column number
(column-number-mode 1)

;; Show beacon aroud cursor on large cursor movements
(beacon-mode 1)

;; Disable emacs' tool-bar and menu-bar
(menu-bar-mode -1)
(tool-bar-mode -1)

;; =============================
;; Configure lsp mode for tilt
;; =============================
(add-to-list 'lsp-language-id-configuration '("Tiltfile$" . "starlack"))
;; (defcustom lsp-starlack-executable "tilt lsp start"
;;   :group 'lsp
;;   :risky t
;;   :type 'file)

(lsp-register-client (make-lsp-client
                      :new-connection (lsp-stdio-connection '("tilt" "lsp" "start"))
                      :activation-fn (lsp-activate-on "starlack")
                      :major-modes '(python-mode)
                      :server-id 'starlack-ls))
(setq lsp-semantic-tokens-enable t)

;; Add resize-
(load (expand-file-name "include/resize-window" user-emacs-directory))
;; Configure AI stuff
;; define ollama language model ids
(require 'llm)
(require 'llm-ollama)
;; contains a list of pairs, each pair (a b) consisting of a: elisp_model_id b: ollama_model_reference

;; Include ai code assistent aissist
(load (expand-file-name "include/aissist" user-emacs-directory))

;; (register_ollama_llm "wizardcoder-33b" "wizardcoder:33b-v1.1-q4_1")
;; (register_ollama_llm "gemma2-9b" "gemma2:9b")
;; (register_ollama_llm "gemma2-27b" "gemma2:27b")
;; (register_ollama_llm "codellama" "codellama:latest")
;; (register_ollama_llm "zephyr-llm" "zephyr:latest")
;; (register_ollama_llm "wizardcoder-15b" "jcdickinson/wizardcoder:latest")
(aissist-init)
;;(aissist-init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aissist-accurate-model wizardcoder-33b)
 '(aissist-fast-model wizardcoder-33b))

;; enable debug logs of the llm package
(setq llm-log "enable")

(use-package kubernetes
  :ensure t
  :straight (kubernetes :type git :host github :repo "kubernetes-el/kubernetes-el")
  :init
  (require 'kubernetes))

;;(unload-feature 'ellama t)
(use-package ellama
  :ensure t
  :straight (ellama :type git :host github :repo "emacs-straight/ellama")
  :init
  :custom (ellama-language "English")
  (require 'llm-ollama)
  :custom (ellama-providers ollama-llm-providers))

;; manage underlying system's packages
(use-package system-packages
  :ensure t
  :straight (system-packages :type git :host gitlab :repo "jabranham/system-packages")
  :init
  (require 'system-packages))

(defun delete-line () "Deletes the whole line but avoiding to insert it into kill-ring." (interactive) (delete-region (line-beginning-position) (line-end-position))
       (delete-char 1))

(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-word (- arg)))

(defun mark-whole-line ()
  "Mark the whole current line and keep point at its original location."
  (interactive)
  (let ((orig-point (point)))
    ;; Move to the beginning of the line.
    (beginning-of-line)
    ;; Set the mark at the beginning of the line.
    (set-mark (point))
    ;; Move to the end of the line and move one character right
    ;; in case point is at the very last position of the buffer,
    ;; so that the newline character is also included if it exists.
    (end-of-line)
    ))

(global-set-key (kbd "M-l") 'mark-whole-line)

(global-set-key (kbd "C-s-e") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-x C-u") 'undo-only)
(global-set-key (kbd "C-_") 'undo-only) ;; For some reason this binds to "C-/" in kitty
(global-set-key (kbd "C-x u") 'undo-only)
(global-set-key (kbd "C-x C-b") 'helm-mini)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-d") 'duplicate-line)

;; Delete stuff without adding to kill ring
;; TODO: fix issues when usin kitty
(global-set-key (kbd "C-k") 'delete-line)
(global-set-key (kbd "C-<backspace>") 'backward-delete-word)
(global-set-key (kbd "C-<backspace>") 'backward-delete-word)
(global-set-key (kbd "C-<delete>") 'delete-word)
