;; example configuration for mu4e
(message "Reading mu4e-conf.el")

(require 'mu4e)

;; (use-package mu4e
;;	     :straight 
;;	       (mu4e
;;	     	:files "/usr/local/share/emacs/site-lisp/mu4e"))
	     
;; use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent)

;; the next are relative to the root maildir
;; (see `mu info`).
;; instead of strings, they can be functions too, see
;; their docstring or the chapter 'Dynamic folders'
(setq mu4e-sent-folder   "/Sent"
      mu4e-drafts-folder "/Drafts"
      mu4e-trash-folder  "/Trash"
      mu4e-refile-folder "/Archive")

;; the maildirs you use frequently; access them with 'j' ('jump')
(setq   mu4e-maildir-shortcuts
    '((:maildir "/Archive" :key ?a)
      (:maildir "/INBOX"   :key ?i)
      (:maildir "/Sent"    :key ?s)))

;; the headers to show in the headers list -- a pair of a field
;; and its width, with `nil' meaning 'unlimited'
;; (better only use that for the last field.

;; These are the defaults:
(setq mu4e-headers-fields
    '( (:date          .  25)    ;; alternatively, use :human-date
       (:flags         .   6)
       (:from          .  22)
       (:subject       .  nil))) ;; alternatively, use :thread-subject

;; If you get your mail without an explicit command,
;; use "true" for the command (this is the default)
;; (setq mu4e-get-mail-command "fdm -kf ~/mailtest/.fdm.conf fetch")
;;      mu4e-update-interval 120)
(setq mu4e-get-mail-command "offlineimap")

;; general emacs mail settings; used when composing e-mail
;; the non-mu4e-* stuff is inherited from emacs/message-mode
(setq mu4e-compose-reply-to-address "XXX"
      user-mail-address "XXX"
      user-full-name  "xxx")

(setq mu4e-compose-signature
      (concat
       "Alex Dijk\n"
       "XXX"))

(require 'smtpmail)
;; smtp mail setting
(setq
   send-mail-function 'smtpmail-send-it
   smtpmail-smtp-server "smtp.freedom.nl"
   smtpmail-default-smtp-server "smtp.freedom.nl"
   smtpmail-smtp-service 587
   smtpmail-stream-type 'starttls

   smtpmail-queue-mail t
   smtpmail-queue-dir "/home/xxx/Documents/Maildir/Queue/cur")

(setq smtpmail-debug-info t)
(setq smtpmail-debug-verb t)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)
