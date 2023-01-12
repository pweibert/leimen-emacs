;;; leerzeichen-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "leerzeichen" "leerzeichen.el" (0 0 0 0))
;;; Generated autoloads from leerzeichen.el

(autoload 'leerzeichen-mode "leerzeichen" "\
Minor mode to highlight whitespace characters by displaying them differently.

If called interactively, enable Leerzeichen mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "leerzeichen" '("leerzeichen-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; leerzeichen-autoloads.el ends here
