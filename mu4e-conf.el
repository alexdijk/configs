;; example configuration for mu4e
(add-to-list 'load-path
             "/usr/local/share/emacs/site-lisp/mu4e")

(use-package mu4e)

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
      (:maildir "/Inbox"   :key ?i)
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

;; program to get mail; alternatives are 'fetchmail', 'getmail'
;; isync or your own shellscript. called when 'U' is pressed in
;; main view.

;; If you get your mail without an explicit command,
;; use "true" for the command (this is the default)
(setq mu4e-get-mail-command "fdm -kf ~/mailtest/.fdm.conf fetch")
;;      mu4e-update-interval 120)

;; general emacs mail settings; used when composing e-mail
;; the non-mu4e-* stuff is inherited from emacs/message-mode
(setq mu4e-compose-reply-to-address "alex.dijk@freedom.nl"
      user-mail-address "alex.dijk@freedom.nl"
      user-full-name  "Alex Dijk")
(setq mu4e-compose-signature
      "Alex Dijk\n\nOpenPGP: 60A4 91B1 BD69 85AE D4CC B83C E728 DBFB B747 31C5\n")

;; smtp mail setting
(setq
   message-send-mail-function 'smtpmail-send-it
   ;;smtpmail-default-smtp-server "smtp.freedom.nl"
   smtpmail-smtp-server "smtp.freedom.nl"
;;   smtpmail-local-domain "example.com"

   ;; if you need offline mode, set these -- and create the queue dir
   ;; with 'mu mkdir', i.e.. mu mkdir /home/user/Maildir/queue
   ;;smtpmail-queue-mail  nil
   ;;smtpmail-queue-dir  "/home/user/Maildir/queue/cur")

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)
