;; .emacs.d/init.el

;; Save backup files to backup directory only
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))

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

;; If there are no archived package contets, refresh them

(when (not package-archive-contents)
  (package-refresh-contents))

;; Initializes the package infrastructure
(package-initialize)


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
    kubernetes
    leerzeichen
    lsp-jedi
    lsp-mode
    lsp-treemacs
    lsp-ui
    magit;; Git integration
    markdown-mode
    multiple-cursors
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
    vterm
    web-mode
    winum
    which-key
    yaml
    yaml-mode
    yasnippet))

;; Scans the list in myPackages
;; If the package listed is not already installed, install it

(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      myPackages)

(transient-mark-mode 1)

;; Set up multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(require 'origami)
(require 'multi-vterm)

(setq vterm-max-scrollback 50000)

;; Enable clipboard or copy/paste integration
(setq select-enable-clipboard t)
(defun copy-to-clipboard ()
  (interactive)
  (if (display-graphic-p)
      (progn
        (message "Yanked region to x-clipboard!")
        (call-interactively 'clipboard-kill-ring-save)
        )
    (if (region-active-p)
        (progn
          (shell-command-on-region (region-beginning) (region-end) "xsel -i -b")
          (message "Yanked region to clipboard!")
          (deactivate-mark))
      (message "No region active; can't yank to clipboard!")))
  )
(defun paste-from-clipboard ()
  (interactive)
  (if (display-graphic-p)
      (progn
        (clipboard-yank)
        (message "graphics active")
        )
    (insert (shell-command-to-string "xsel -o -b"))
    )
  )
(global-set-key [f8] 'copy-to-clipboard)
(global-set-key [f9] 'paste-from-clipboard)


;; Ediff preferences
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-keep-variants nil)
(setq ediff-merge-revisions-with-ancestor t)
(setq ediff-make-buffers-readonly-at-startup nil)

(require 'browse-kill-ring)

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

;;(setq-default dired-listing-switches "-alh1")

;; Enable autopep-8
;;(require 'py-autopep8)

;; Enable visualization of whitespaces
(require 'leerzeichen)

;(global-set-key [C-tab] 'elpy-company-backend)

(windmove-default-keybindings) ;; Move cursor between windows
;(setq elpy-get-info-from-shell t)

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
   '(which-key web-mode undo-tree tide spacemacs-theme magit lsp-ui lsp-jedi leerzeichen kubernetes k8s-mode json-mode js2-mode helm-xref helm-projectile helm-lsp elpy drag-stuff dockerfile-mode dap-mode better-defaults))
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

