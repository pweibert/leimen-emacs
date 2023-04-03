;; .emacs.d/init.el

;; Save backup files to backup directory only
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))

;; ===================================

;; MELPA Package Support

;; ===================================

;; Enables basic packaging support
(require 'package)


;; Adds the Melpa archive to the list of available repositories

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)


;; Initializes the package infrastructure

(package-initialize)


;; If there are no archived package contets, refresh them

(when (not package-archive-contents)
  (package-refresh-contents))
;; Installs 
;; myPackages contains a list of package names

(defvar myPackages
  '(better-defaults                 ;; Set up some better Emacs defaults
    elpy                            ;; Emacs Lisp Python Environment
    flycheck                        ;; On the fly syntax checking
    helm-projectile                 ;; Look for files in project
    spacemacs-theme
    magit                           ;; Git integration
    py-autopep8                     ;; Code formatting
    lsp-mode
    lsp-treemacs
    lsp-ui
    yasnippet
    helm-lsp
    helm-xref
    projectile
    hydra
    company
    avy
    which-key
    dap-mode
    json-mode
    js2-mode
    web-mode
    tide
    drag-stuff
    leerzeichen
    )
  )

;; Scans the list in myPackages

;; If the package listed is not already installed, install it

(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      myPackages)

(transient-mark-mode 1)
;; ===================================

;; Basic Customization

;; ===================================

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-message t)    ;; Hide the startup message

(load-theme 'spacemacs-dark t)      ;; Load theme

(global-linum-mode t)               ;; Enable line numbers globally
(helm-mode)

(require 'drag-stuff)               ;; Moving words and regions
(drag-stuff-define-keys)
(drag-stuff-global-mode)            ;; Enable dragging everywhere

(require 'helm-projectile)
(require 'helm-xref)
(require 'org)
(require 'web-mode)

(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)

(which-key-mode)
(add-hook 'prog-mode-hook #'lsp)
(setq gc-cons-threshold (* 100 1024 1024)
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      create-lockfiles nil) ;; lock files will kill npm start

(with-eval-after-load 'lsp-mode
  (require 'dap-chrome)
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (yas-global-mode))

;; (with-eval-after-load 'js
;;   (define-key js-mode-map (kdb "M-.") nil))
;; (with-eval-after-load 'js2-mode
;;   (define-key js2-mode-map (kdb "M-.") nil)
;; )
(require 'flycheck) 
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "jsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
(add-to-list 'auto-mode-alist '("Tiltfile'" . mode-yaml))


(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)

;; User-Defined init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("90a6f96a4665a6a56e36dec873a15cbedf761c51ec08dd993d6604e32dd45940" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(elpy-formatter 'autopep8)
 '(package-selected-packages
   '(lsp-mode js2-mode rjsx-mode kubernetes yaml-mode emr company-tabnine undo-tree dockerfile-mode k8s-mode markdown-mode markdown-preview-mode spacemacs-theme better-defaults cmake-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(setq company-idle-delay 0)
(setq company-show-numbers t)
;; ====================================

;; Development Setup

;; ====================================
;; Kubernetes
(setq kubernetes-poll-frequency 3600)
(setq kubernetes-redraw-frequency 3600)

;; Enable elpy
(elpy-enable)
;; Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(require 'tide)

;; configure jsx-tide checker to run after your default jsx checker
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)

;; Make sure jedi parses the same python packages that are installed in python3.9
;; (setenv "PYTHONPATH" (concat (expand-file-name "~/.local/lib/python3.9/site-packages/:") (getenv "PYTHONPATH")))
;; Set python interpreter to use
;;(setq python-shell-interpreter "python3.9")

;;(setq-default dired-listing-switches "-alh1")

;; Enable autopep-8
(require 'py-autopep8)

;; Enable visualization of whitespaces
(require 'leerzeichen)

(global-set-key [C-tab] 'elpy-company-backend)

(windmove-default-keybindings) ;; Move cursor between windows
(setq elpy-get-info-from-shell t)

;; Define function to interrupt shell
(defun elpy-shell-interrupt ()
  "Interrupt the process associated to the current python script."
  (interactive)
  (elpy-shell-switch-to-shell)
  (interrupt-process)
  (elpy-shell-switch-to-buffer))

;; Bind interrupt shell finc to shortcut
(global-set-key [C-c i] 'elpy-shell-interrupt)

;; Configure company-tabnine
;; (require 'company-tabnine)
;; (add-to-list 'company-backends #'company-tabnine)

;; Convenience functions for development
(defun update-frontend-pod ()
  "Update frontend web-app files in frontend pod"
  (interactive)
  (message "Update frontend files")
  (shell-command "/home/paul/Development/workspaces/eclipse-workspace/microservices/services/frontend/sync-pod.sh")
  )

(global-set-key [f9] 'update-frontend-pod)
(global-set-key [s-p] 'helm-projectile)

;; Match parentheses
(show-paren-mode 1);; User-Defined init.el ends here

;; Make sure files that change on disk are updated in emacs
(global-auto-revert-mode t)

;; Prevent emacs from creating backup files
(setq make-backup-files nil)

