;; -*- lexical-binding: t; -*-
(use-package mu4e
  :load-path "/opt/homebrew/opt/mu/share/emacs/site-lisp/mu/mu4e/")

(setq user-full-name "Zhixiao Zhang"
      user-mail-address "zhixiao.zhang@gwmail.gwu.edu")

(setq mu4e-maildir "~/.mail/gwu"
      mu4e-sent-folder "/[Gmail]/Sent Mail"
      mu4e-drafts-folder "/Draft"
      mu4e-trash-folder "/[Gmail]/Trash"
      mu4e-get-mail-command "mbsync gwu"
      mu4e-change-filenames-when-moving t
      mu4e-update-interval nil)

(setq send-mail-function 'sendmail-send-it
      message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "msmtp"
      message-sendmail-extra-arguments '("-a" "gwu"))
