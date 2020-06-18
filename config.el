;;; Config.el --- -*- lexical-binding: t -*-
;;; Author: Alex Day
(setq! user-full-name "Alex Day"
       user-full-address "alex@alexday.me")
(auto-image-file-mode 1)
(setq! doom-font "JetBrainsMono Nerd Font Mono-13"
       doom-unicode-font "JoyPixels-14")
(setq custom-safe-themes t)
(load-theme 'doom-gruvbox )
(setq frame-resize-pixelwise t)
(setq show-trailing-whitespace t)
(setq display-line-numbers-type 'relative)
(setq projectile-project-search-path '("~/code/"))
(setq jedi:complete-on-dot t)
(add-hook! 'image-mode-hook 'eimp-mode)
(setq org-directory "~/Dropbox/")
(require 'org-protocol)
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
      '(("t" "Todo" entry (file+olp+datetree "~/Dropbox/todo/todo.org" "Inbox")
         "* TODO %?\n  %i\n  %a")
        ("z" "Website Capture" entry (file+headline "~/Dropbox/todo/todo.org" "Inbox")
    "* %:annotation\n %:initial\n %u\n\n\n%?")
       ("e" "Email Todo" entry (file+olp+datetree "~/Dropbox/todo/todo.org" "Inbox"))
        ("e" "Email Todo" entry (file+olp+datetree "~/Dropbox/todo/todo.org" "Inbox")
         "* TODO %?\nProcess mail from %:fromname on %:subject\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n:PROPERTIES:\n:CREATED: %U\n:END:\n %a" :prepend t)))
(setq org-agenda-files '("~/Dropbox/notes/" "~/Dropbox/todo/"))
(setq org-pretty-entities 't)
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(after! mu4e
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
                                                       ("/clemson/sent"   . ?s)
                                                       ("/clemson/trash"       . ?t)
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
                ( mu4e-drafts-folder     . "/clemson/drafts")
                ( mu4e-sent-folder       . "/clemson/sent")
                ( mu4e-trash-folder      . "/clemson/trash")
                ( mu4e-refile-folder     . "/clemson/archive" )
                ( mu4e-compose-signature .
                    (concat
                    "Alex Day"))))
       ,(make-mu4e-context
        :name "gmail"
        :enter-func (lambda ()
                      (mu4e-message "Entering Gmail context")
                      (setq mu4e-maildir-shortcuts  '( ("/gmail/INBOX"               . ?i)
                                                       ("/gmail/sent"   . ?s)
                                                       ("/gmail/trash"       . ?t)
                                                       ("/gmail/archive"             . ?r)))
                    )
        :leave-func (lambda () (mu4e-message "Leaving Gmail context"))
        :match-func (lambda (msg)
                        (when msg
                                (or (mu4e-message-contact-field-matches msg
                                        :to "alexday135@gmail.com")
                                    (mu4e-message-contact-field-matches msg
                                        :to "A.D.Day@eagle.clarion.edu"))))
        :vars '( ( user-mail-address     . "alexday135@gmail.com"  )
                ( user-full-name         . "Alex Day" )
                ( mu4e-drafts-folder     . "/gmail/drafts")
                ( mu4e-sent-folder       . "/gmail/sent")
                ( mu4e-trash-folder      . "/gmail/trash")
                ( mu4e-refile-folder     . "/gmail/archive" )
                ( mu4e-compose-signature .
                    (concat
                    "Alex Day"))))))


;; Add bookmarks
(setq mu4e-bookmarks
  `( ,(make-mu4e-bookmark
       :name "Messages in inbox"
       :query "maildir:\"/clemson/INBOX\" OR maildir:\"/gmail/INBOX\""
       :key ?i)
     ,(make-mu4e-bookmark
       :name  "Unread messages"
       :query "flag:unread AND NOT flag:trashed"
       :key ?u)
     ,(make-mu4e-bookmark
       :name "Today's messages"
       :query "date:today..now"
       :key ?t)
     ,(make-mu4e-bookmark
       :name "Last 7 days"
       :query "date:7d..now"
       :key ?w)))
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
(setq mu4e-get-mail-command "mbsync -a")

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
(setq org-mu4e-link-query-in-headers-mode nil))
(setq deft-extensions '("org"))
(setq deft-directory "~/Dropbox/notes")
(setq deft-recursive t)
(setq org-roam-directory "~/Dropbox/notes")
(setq org-roam-index-file "~/Dropbox/notes/index.org")
(add-hook 'after-init-hook 'org-roam-mode)
(server-start)
(setq org-roam-graph-viewer "/usr/bin/brave")
(require 'org-roam-protocol)

(after! org-roam
      (setq org-roam-ref-capture-templates
            '(("r" "ref" plain (function org-roam-capture--get-point)
               "%?"
               :file-name "${slug}"
               :head "#+TITLE: ${title}
    #+ROAM_KEY: ${ref}
    - source :: ${ref}"
               :unnarrowed t))))
(setq org-ref-default-bibliography '("~/Dropbox/papers/references.bib"))
 (setq
 bibtex-completion-notes-path "~/Dropbox/notes"
 bibtex-completion-bibliography "~/Dropbox/papers/references.bib"
 bibtex-completion-pdf-field "file"
 bibtex-completion-notes-template-multiple-files
 (concat
  "#+TITLE: ${title}\n"
  "#+ROAM_KEY: cite:${=key=}\n\n"
  "* TODO Notes\n"
  ":PROPERTIES:\n"
  ":Custom_ID: ${=key=}\n"
  ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
  ":AUTHOR: ${author-abbrev}\n"
  ":JOURNAL: ${journaltitle}\n"
  ":DATE: ${date}\n"
  ":YEAR: ${year}\n"
  ":DOI: ${doi}\n"
  ":URL: ${url}\n"
  ":END:\n\n"
  )
 )
 (use-package org-roam-bibtex
  :after (org-roam)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq org-roam-bibtex-preformat-keywords
   '("=key=" "title" "url" "file" "author-or-editor" "keywords"))
  (setq orb-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "${slug}"
           :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}

- tags ::
- keywords :: ${keywords}
\n* ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :URL: ${url}\n  :AUTHOR: ${author-or-editor}\n  :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n  :NOTER_PAGE: \n  :END:\n\n"

           :unnarrowed t
           :immediate-finish t))))
(use-package org-noter
  :after (:any org pdf-view)
  :config
  (setq
   ;; Emacs can handle splits
   org-noter-notes-window-location 'horizontal-split
   ;; Please stop opening frames
   org-noter-always-create-frame nil
   ;; I want to see the whole file
   org-noter-hide-other nil
   ;; Everything is relative to the main notes file
   org-noter-notes-search-path '("~/Dropbox/notes")
   )
  )
(after! pdf-view
  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-width)
  ;; automatically annotate highlights
  (setq pdf-annot-activate-created-annotations t
        pdf-view-resize-factor 1.1)
   ;; faster motion
 (map!
   :map pdf-view-mode-map
   :n "g g"          #'pdf-view-first-page
   :n "G"            #'pdf-view-last-page
   :n "N"            #'pdf-view-next-page-command
   :n "E"            #'pdf-view-previous-page-command
   :n "e"            #'evil-collection-pdf-view-previous-line-or-previous-page
   :n "n"            #'evil-collection-pdf-view-next-line-or-next-page
   :n "i"            #'org-noter-insert-note ))
(setq bibtex-completion-library-path '("~/Dropbox/papers"))
;; (helm-delete-action-from-source "Edit notes" helm-source-bibtex)
;; (helm-add-action-to-source "Edit notes" 'bibtex-completion-edit-notes helm-source-bibtex 0)
(setq! +latex-viewers '(pdf-tools)
       TeX-view-evince-keep-focus 't)
(add-hook! 'latex-mode-hook
           (setq line-mode-visual nil))
(add-hook! 'python-mode-hook
           (add-to-list 'company-backends 'company-jedi))
(require 'python-docstring)
(add-hook 'python-mode-hook 'python-docstring-minor-mode)
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
(map! :leader
      (:prefix ("o")
        :desc "Open todo.org" "t" (lambda () (interactive) (find-file "~/Dropbox/todo/todo.org"))))
(map! :leader
      (:prefix ("o")
        :desc "Open mu4e" "m" 'mu4e))
(map! :leader
      (:prefix ("d" . "org roam")
        :desc "backlinks" "l" 'org-roam
        :desc "jump to index file" "x" 'org-roam-jump-to-index
        :desc "find file" "d" 'deft
        :desc "insert file" "i" 'org-roam-insert
        :desc "noter" "n" 'org-noter
        :desc "view bibliography" "b" 'helm-bibtex
        :desc "view todays note" "t" 'org-roam-dailies-today
        :desc "view tomorrows note" "m" 'org-roam-dailies-tomorrow
        :desc "insert cite" "c" 'org-ref-helm-insert-cite-link))
;; (map! :leader
;;       (:prefix ("d" . "deft")
;;         :desc "deft" "d" 'deft
;;         :desc "new search" "D" 'zetteldeft-deft-new-search
;;         :desc "refresh" "R" 'deft-refresh
;;         :desc "search at point" "s" 'zetteldeft-search-at-point
;;         :desc "search current id" "c" 'zetteldeft-search-current-id
;;         :desc "follow link" "f" 'zetteldeft-follow-link
;;         :desc "avy file other window" "F" 'zetteldeft-avy-file-search-ace-window
;;         :desc "avy link search" "l" 'zetteldeft-avy-link-search
;;         :desc "avy tag search" "t" 'zetteldeft-avy-tag-search
;;         :desc "tag list" "T" 'zetteldeft-tag-buffer
;;         :desc "insert id" "i" 'zetteldeft-find-file-id-insert
;;         :desc "insert full title" "I" 'zetteldeft-find-file-full-title-insert
;;         :desc "find file" "o" 'zetteldeft-find-file
;;         :desc "new file" "n" 'zetteldeft-new-file
;;         :desc "new file & link" "N" 'zetteldeft-new-file-and-link
;;         :desc "rename" "r" 'zetteldeft-file-rename
;;         :desc "count words" "x" 'zetteldeft-count-words))
(add-hook! 'evil-org-mode-hook
    (evil-define-key 'normal evil-org-mode-map
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line))
(define-key org-noter-doc-mode-map (kbd "i") 'org-noter-insert-note)
