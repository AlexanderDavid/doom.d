;;; Config.el --- -*- lexical-binding: t -*-
;;; Author: Alex Day
(setq! user-full-name "Alex Day"
       user-full-address "alex@alexday.me")
(setq! doom-font "JetBrainsMono Nerd Font Mono-13"
       doom-unicode-font "JoyPixels-14")
(load-theme 'doom-gruvbox)
(setq frame-resize-pixelwise t)
(setq show-trailing-whitespace t)
(setq display-line-numbers-type 'relative)
(setq projectile-project-search-path '("~/code/"))
(setq shell-file-nae "/bin/sh")
(setq jedi:complete-on-dot t)
(add-hook! 'image-mode-hook 'eimp-mode)
(setq org-directory "~/doc/org/")
(setq org-agenda-files '("~/doc/org/"))
(setq org-ellipsis " ▼ ")
(setq org-fontify-done-headline t)
(custom-set-faces
 '(org-done ((t (
                 :weight bold
                 :strike-through nil))))
 '(org-headline-done
   ((((class color) (min-colors 16) (background dark))
     (:strike-through t)))))
(require 'org-mu4e)
(setq org-capture-templates
      '(("p" "Todo" entry (file+headline "~/doc/org/todo.org" "Inbox")
         "* TODO %?\n  %a\n")
        ;; ("e" "Email Todo" entry (file+headline "~/doc/org/todo.org" "Inbox")
        ;;  "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
        ("e" "Email Todo" entry (file+headline "~/doc/org/todo.org" "Inbox")
         "* TODO %?\nProcess mail from %:fromname on %:subject\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n:PROPERTIES:\n:CREATED: %U\n:END:\n %a" :prepend t)
        ("n" "Note" entry (file+headline "~/doc/org/notes.org" "Notes")
         "* NOTE %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/doc/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-start-day "0d")
(setq org-agenda-span 'day)
(setq org-pretty-entities 't)
(require 'mu4e)

;; use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent)
(setq mu4e-maildir "/home/alex/.local/share/mail")

;; default
(setq mu4e-contexts
    `( ,(make-mu4e-context
        :name "clemson"
        :enter-func (lambda ()
                      (mu4e-message "Entering Clemson context")
                      (setq mu4e-maildir-shortcuts  '( ("/clemson/INBOX"               . ?i)
                                                       ("/clemson/[Gmail].Sent Mail"   . ?s)
                                                       ("/clemson/[Gmail].Trash"       . ?t)
                                                       ("/clemson/[Gmail].All Mail"    . ?a)
                                                       ("/clemson/archive"             . ?r)))
                    )
        :leave-func (lambda () (mu4e-message "Leaving Clemson context"))
        :match-func (lambda (msg)
        (when msg
            (or (mu4e-message-contact-field-matches msg
                :to "adday@clemson.edu")
                (mu4e-message-contact-field-matches msg
                :to "adday@g.clemson.edu"))))
        :vars '( ( user-mail-address     . "adday@clemson.edu"  )
                ( user-full-name         . "Alex Day" )
                ( mu4e-drafts-folder     . "/clemson/[Gmail].Drafts")
                ( mu4e-sent-folder       . "/clemson/[Gmail].Sent Mail")
                ( mu4e-trash-folder      . "/clemson/[Gmail].Trash")
                ( mu4e-refile-folder     . "/clemson/archive" )
                ( mu4e-compose-signature .
                    (concat
                    "Alex Day"))))
       ,(make-mu4e-context
        :name "Gmail"
        :enter-func (lambda ()
                      (mu4e-message "Entering Gmail context")
                      (setq mu4e-maildir-shortcuts  '( ("/gmail/INBOX"               . ?i)
                                                       ("/gmail/[Gmail].Sent Mail"   . ?s)
                                                       ("/gmail/[Gmail].Trash"       . ?t)
                                                       ("/gmail/[Gmail].All Mail"    . ?a)
                                                       ("/gmail/archive"             . ?r)))
                    )
        :leave-func (lambda () (mu4e-message "Leaving Gmail context"))
        :match-func (lambda (msg)
                        (when msg
                                (mu4e-message-contact-field-matches msg
                                :to "alexday135@gmail.com")))
        :vars '( ( user-mail-address     . "alexday135@gmail.com"  )
                ( user-full-name         . "Alex Day" )
                ( mu4e-drafts-folder     . "/gmail/[Gmail].Drafts")
                ( mu4e-sent-folder       . "/gmail/[Gmail].Sent Mail")
                ( mu4e-trash-folder      . "/gmail/[Gmail].Trash")
                ( mu4e-refile-folder     . "/gmail/archive" )
                ( mu4e-compose-signature .
                    (concat
                    "Alex Day"))))))


;; set `mu4e-context-policy` and `mu4e-compose-policy` to tweak when mu4e should
;; guess or ask the correct context, e.g.

;; start with the first (default) context;
;; default is to ask-if-none (ask when there's no context yet, and none match)
;; (setq mu4e-context-policy 'pick-first)

;; compose with the current context is no context matches;
;; default is to ask
;; (setq mu4e-compose-context-policy nil)

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap && mu index --maildir=.local/share/mail/ --muhome=.cache/mu")

;; Download attachments to the correct directory
(setq mu4e-attachment-dir "~/dl")

;; Sometimes html email is just not readable in a text based client, this lets me open the
;; email in my browser.
(add-to-list 'mu4e-view-actions '("View in browser" . mu4e-action-view-in-browser) t)

;; sending mail -- replace USERNAME with your gmail username
;; also, make sure the gnutls command line utils are installed
;; package 'gnutls-bin' in Debian/Ubuntu


(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "/usr/bin/msmtp")
;; tell msmtp to choose the SMTP server according to the from field in the outgoing email
(setq message-sendmail-extra-arguments '("--read-envelope-from"))
(setq message-sendmail-f-is-evil 't)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; Store link to message if in header view, not to header query
(setq org-mu4e-link-query-in-headers-mode nil)
(setq! +latex-viewers '(pdf-tools)
       TeX-view-evince-keep-focus 't)
(add-hook! 'latex-mode-hook
           (setq line-mode-visual nil))
(add-hook! 'python-mode-hook
           (add-to-list 'company-backends 'company-jedi))
(defun python-args-to-google-docstring (text &optional make-fields)
  "Return a reST docstring format for the python arguments in yas-text."
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
         (args (python-split-args text))
     (nr 0)
         (formatted-args
      (mapconcat
       (lambda (x)
         (concat "   " (nth 0 x)
             (if make-fields (format " ${%d:arg%d}" (cl-incf nr) nr))
             (if (nth 1 x) (concat " \(default " (nth 1 x) "\)"))))
       args
       indent)))
    (unless (string= formatted-args "")
      (concat
       (mapconcat 'identity
          (list "" "Args:" formatted-args)
          indent)
       "\n"))))
(defun run-python-script ()
  (interactive)
  (shell-command (format "python %s" (buffer-name)) "*python-output*"))
(global-set-key [f5] 'run-python-script)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(map! :leader
  (:prefix ("r" . "replace")
  :desc "String" "s" 'replace-string
  :desc "Query" "q" 'query-replace
  (:prefix ("r" . "Regexp")
    :desc "String" "s" 'replace-regexp
    :desc "Query" "q" 'query-replace-regexp
    )
  )
)
(map! :leader
      (:prefix ("i" . "insert")
       :desc "Unicode" "u" 'insert-char
       :desc "Snippet" "s" 'yas-insert-snippet
       :desc "From Clipboard" "y" '+default/yank-pop
       :desc "From Evil Registers" "r" 'counsel-evil-registers
      )
)
(map! :localleader
      :map eimp-minor-mode-map
      (:prefix ("z" . "zoom")
        :desc "In" "i" 'eimp-increase-image-size
        :desc "Out" "o" 'eimp-decrease-image-size
        :desc "Fit to Window" "f" 'eimp-fit-image-to-window))
