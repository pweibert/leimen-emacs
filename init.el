;; .emacs.d/init.el


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
;; Installs packages

;;

;; myPackages contains a list of package names

(defvar myPackages
  '(better-defaults                 ;; Set up some better Emacs defaults
    elpy                            ;; Emacs Lisp Python Environment
    flycheck                        ;; On the fly syntax checking
    ;;material-theme                  ;; Theme
    spacemacs-theme
    magit                           ;; Git integration
    py-autopep8                     ;; Run autopep8 on save
    )
  )


;; Scans the list in myPackages

;; If the package listed is not already installed, install it

(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      myPackages)

;; ===================================

;; Basic Customization

;; ===================================


(setq inhibit-startup-message t)    ;; Hide the startup message

(load-theme 'spacemacs-dark t)      ;; Load theme

(global-linum-mode t)               ;; Enable line numbers globally


;; User-Defined init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("90a6f96a4665a6a56e36dec873a15cbedf761c51ec08dd993d6604e32dd45940" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(package-selected-packages
   '(yaml-mode emr company-tabnine undo-tree dockerfile-mode k8s-mode markdown-mode markdown-preview-mode spacemacs-theme better-defaults cmake-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; ====================================

;; Development Setup

;; ====================================

;; Enable elpy
(elpy-enable)
;; Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

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
(windmove-default-keybindings)
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
(require 'company-tabnine)
(add-to-list 'company-backends #'company-tabnine)

;; Match parentheses
(show-paren-mode 1)
;; User-Defined init.el ends here

