;;; py-autopep8-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "py-autopep8" "py-autopep8.el" (0 0 0 0))
;;; Generated autoloads from py-autopep8.el

(autoload 'py-autopep8-buffer "py-autopep8" "\
Use the \"autopep8\" tool to reformat the current buffer.
Return non-nil when a the buffer was modified." t nil)

(autoload 'py-autopep8-region "py-autopep8" "\
Use the \"autopep8\" tool to reformat whole lines in the region (BEG, END).
Return non-nil when a the buffer was modified.

\(fn BEG END)" t nil)

(autoload 'py-autopep8-mode "py-autopep8" "\
Py-autopep8 minor mode.

If called interactively, enable Py-Autopep8 mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "py-autopep8" '("py-autopep8-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; py-autopep8-autoloads.el ends here
