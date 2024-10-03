;;; mu4e-conf.el --- mu4e configuration file  -*- lexical-binding: t; -*-
;;; package --- Summary: mu4e-conf.el
;;; Commentary:
;;; elementary configuration file for the mail package mu/mu4e
;;;
;;; Code:
(require 'mu4e)
(require 'smtpmail)

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

;; These are the defaults:
(setq mu4e-headers-fields
      '((:date         .  12)    ;; alternatively, use :human-date
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
(setq mu4e-compose-reply-to-address "alex.dijk@freedom.nl"
      user-mail-address "alex.dijk@freedom.nl"
      user-full-name  "Alex Dijk")

(setq mu4e-compose-signature
      (concat
       "Alex Dijk\n\n"
       "m: +31-6-52607033\n"
       "e: alex.dijk@freedom.nl"
       "openpgp: E728DBFBB74731C5"))

;; smtpmail settings
(setq
   send-mail-function 'smtpmail-send-it
   smtpmail-smtp-server "smtp.freedom.nl"
   smtpmail-default-smtp-server "smtp.freedom.nl"
   smtpmail-servers-requiring-authorization "smtp.freedom.nl"
   smtpmail-smtp-service 587
   smtpmail-stream-type 'starttls

   ;; need offline mode, set these -- and create the queue dir
   ;; with 'mu mkdir', i.e.. mu mkdir /home/user/Maildir/queue
   smtpmail-queue-mail nil
   smtpmail-queue-dir "/home/alexdijk/Documents/Maildir/Queue/cur")

(setq smtpmail-debug-info nil)
(setq smtpmail-debug-verb nil)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; nice columns in mu4e
(use-package mu4e-column-faces
  :straight t
  :after mu4e
  :config (mu4e-column-faces-mode))

(provide 'mu4-conf)
;;; mu4e-conf.el ends here
