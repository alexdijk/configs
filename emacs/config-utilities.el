;;
;; Utility functions for emacs
;;
;; Little vi hack: match parenthesis with '%'
(global-set-key "%" 'ad/match-paren)
(defun ad/match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(defvar ad/help-window-names
  '(
    ;; Ubiquitous help buffers
    "*Help*"
    "*Apropos*"
    "*Messages*"
    "*Completions*"
    ;; Other general buffers
    "*Command History*"
    "*Compile-Log*"
    "*disabled command*")
  "Names of buffers that `my/quit-help-windows' should quit.")

(defun ad/quit-help-windows (&optional kill frame)
  "Quit all windows with help-like buffers.
Call `quit-windows-on' for every buffer named in
`my/help-windows-name'.  The optional parameters KILL and FRAME
are just as in `quit-windows-on', except FRAME defaults to t (so
that only windows on the selected frame are considered).

Note that a nil value for FRAME cannot be distinguished from an
omitted parameter and will be ignored; use some other value if
you want to quit windows on all frames."
  (interactive)
  (let ((frame (or frame t)))
    (dolist (name my/help-window-names)
      (ignore-errors
        (quit-windows-on name kill frame)))))
