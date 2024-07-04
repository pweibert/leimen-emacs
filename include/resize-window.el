;; resize-window.el --- A minor mode to resize frames easily.  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  kuanyui

;; Author: kuanyui <azazabc123@gmail.com>
;; Keywords: frames, tools, convenience
;; License: WTFPL 1.0

;;; Commentary:

;; Press "ESC `" and use arrow-keys to adjust frames. press any key to done.

;;; Code:

(defvar resize-window-map
  (let ((map (make-keymap)))
    ;; arrow keys not working for some reason
    (define-key map (kbd "<up>") 'enlarge-window)
    (define-key map (kbd "<down>") 'shrink-window)
    (define-key map (kbd "<right>") 'enlarge-window-horizontally)
    (define-key map (kbd "<left>") 'shrink-window-horizontally)

    (set-char-table-range (nth 1 map) t 'resize-window-done)
    (define-key map (kbd "C-p") 'enlarge-window)
    (define-key map (kbd "C-n") 'shrink-window)
    (define-key map (kbd "C-f") 'enlarge-window-horizontally)
    (define-key map (kbd "C-b") 'shrink-window-horizontally)
    map))

(define-minor-mode resize-window
  "A simple minor mode to resize-window.
C-c C-c to apply."
  ;; The initial value.
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " ResizeFrame"
  ;; The minor mode bindings.
  :keymap resize-window-map
  :global t
  (if (<= (length (window-list)) 1)
      (progn (setq resize-window nil)
             (message "Only root window exists, abort."))
      (message "Use arrow-keys to adjust window.")))

(defun resize-window-done ()
  (interactive)
  (setq resize-window nil)
  (message "Done."))

;;(global-set-key (kbd "ESC `") 'resize-window)

(provide 'resize-window)
;;; resize-window.el ends here
