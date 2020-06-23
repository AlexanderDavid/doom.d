;;; Config.el --- -*- lexical-binding: t -*-
;;; Author: Alex Day
(setq! user-full-name "Alex Day"
       user-mail-address "alexday135@gmail")
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
(auto-image-file-mode 1)
(setq org-directory "~/Dropbox/gtd/")
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
(setq org-tags-column 50)
(setq org-refile-targets
      '(("tickler.org" :maxlevel . 1)
        ("someday.org" :maxlevel . 1)
        ("projects.org" :maxlevel . 3)))
(require 'org-mu4e)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Dropbox/gtd/inbox.org" "Inbox")
         "* TODO %?\n  %i\n%t\n%a")

        ("d" "Review: Daily Review" entry (file+datetree "~/Dropbox/gtd/reviews.org") (file "~/Dropbox/gtd/templates/dailyreviewtemplate.org"))

        ("T" "Tickler" entry (file+headline "~/Dropbox/gtd/tickler.org" "Tickler")
         "* TODO %?\n  %i\n%t\n%a")

        ("z" "Website Capture" entry (file+headline "~/Dropbox/gtd/inbox.org" "Inbox")
        "* TODO %:annotation\n %:initial\n %u\n\n\n%?")

        ("e" "Email" entry (file+headline "~/Dropbox/gtd/inbox.org" "Inbox")
            "* TODO [#A] Reply: %a %(create-mail-tag)\n%:date-timestamp"
            :immediate-finish t)))

(defun create-mail-tag ()
  (let ((to (plist-get org-store-link-plist :to)))
    (if (equal to "'Alex Day' <alexday135@gmail.com>")
        ":@home:"
        ":@school:")))

    ;; (format "%s" to)))
;; (setq org-capture-templates-contexts
;;       '(("e" (in-mode . "mu4e-headers-mode"))))
        ;; ("e" (in-mode . "mu4e-view-mode"))))
(setq org-capture-templates-contexts
      '(("e" ((in-mode . "mu4e-view-mode")
	      (in-mode . "mu4e-message-mode")
          (in-mode . "mu4e-headers-mode")))))
(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(setq org-agenda-use-time-grid nil)
(setq org-agenda-files (list org-directory))
(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (agenda "")
          (alltodo ""
                   ((org-agenda-skip-function
                     '(or (air-org-skip-subtree-if-priority ?A)
                          (org-agenda-skip-if nil '(scheduled deadline))))))))
          ("h" "Home tasks" tags-todo "HOME"
            ((org-agenda-overriding-header "Home Tasks")))
        ("s" "School tasks" tags-todo "SCHOOL"
         ((org-agenda-overriding-header "School Tasks")))
        ("w" "Work tasks" ((agenda "") (tags-todo "WORK"))
         ((org-agenda-overriding-header "Work Tasks")
          (org-agenda-tag-filter-preset "WORK")))))
(setq org-pretty-entities 't)
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-roam-directory "~/Dropbox/notes")
(setq org-roam-index-file "~/Dropbox/notes/index.org")
(add-hook 'after-init-hook 'org-roam-mode)
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
(setq org-roam-graph-edge-cites-extra-config '(("color" . "red")))
(setq org-ref-default-bibliography '("~/Dropbox/notes/papers/references.bib"))
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
\n* [[%(orb-process-file-field \"${=key=}\")][${title}]]\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :URL: ${url}\n  :AUTHOR: ${author-or-editor}\n  :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n  :NOTER_PAGE: \n  :END:\n\n"

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
(use-package ox-hugo
  :ensure t            ;Auto-install the package from Melpa (optional)
  :after ox)
(setq deft-extensions '("org"))
(setq deft-directory "~/Dropbox/notes")
(setq deft-recursive t)
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
 (setq
 bibtex-completion-notes-path "~/Dropbox/notes"
 bibtex-completion-bibliography "~/Dropbox/notes/papers/references.bib"
 bibtex-completion-pdf-field "file"
 bibtex-completion-library-path '("~/Dropbox/notes/papers")
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
(after! helm-bibtex
    (helm-delete-action-from-source "Edit notes" helm-source-bibtex)
    (helm-add-action-to-source "Edit notes" 'helm-bibtex-edit-notes helm-source-bibtex 0))
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
(after! org
  (after! org-ref
    (after! ox-hugo
        (add-to-list 'org-ref-formatted-citation-formats
                    '("md"
                        ("article" . "${author}, *${title}*, ${journal}, *${volume}(${number})*, ${pages} (${year}). ${doi}")
                        ("inproceedings" . "${author}, *${title}*, ${editor}, ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
                        ("book" . "${author}, *${title}* (${year}), ${address}: ${publisher}.")
                        ("phdthesis" . "${author}, *${title}* (Doctoral dissertation) (${year}). ${school}, ${address}.")
                        ("inbook" . "${author}, *${title}*, In ${editor} (Eds.), ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
                        ("incollection" . "${author}, *${title}*, In ${editor} (Eds.), ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
                        ("proceedings" . "${editor} (Eds.), _${booktitle}_ (${year}). ${address}: ${publisher}.")
                        ("unpublished" . "${author}, *${title}* (${year}). Unpublished manuscript.")
                        ("misc" . "${author} (${year}). *${title}*. Retrieved from [${howpublished}](${howpublished}). ${note}.")
                        (nil . "${author}, *${title}* (${year})."))))))
(require 'org-download)
(setq-default org-download-image-dir "~/Dropbox/notes/images")
(setq-default org-download-heading-lvl nil)
(setq org-download-screenshot-method "maim -s -d 0.1 %s")
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
        :desc "Open inbox.org" "i" (lambda () (interactive) (find-file "~/Dropbox/gtd/inbox.org"))
        :desc "Open tickler.org" "t" (lambda () (interactive) (find-file "~/Dropbox/gtd/tickler.org"))
        :desc "Open someday.org" "s" (lambda () (interactive) (find-file "~/Dropbox/gtd/someday.org"))
        :desc "Open projects.org" "p" (lambda () (interactive) (find-file "~/Dropbox/gtd/projects.org"))))
(map! :leader
      (:prefix ("o")
        :desc "Open mu4e" "m" 'mu4e))
(map! :leader
      (:prefix ("d" . "org roam")
        :desc "backlinks" "l" 'org-roam
        :desc "jump to index file" "x" 'org-roam-jump-to-index
        :desc "find file" "d" 'deft
        :desc "new file" "f" 'org-roam-find-file
        :desc "show graph" "g" 'org-roam-graph
        :desc "insert file" "i" 'org-roam-insert
        :desc "noter" "n" 'org-noter
        :desc "view bibliography" "b" 'helm-bibtex
        :desc "Insert screenshot" "s" 'org-download-screenshot
        :desc "insert cite" "c" 'org-ref-helm-insert-cite-link))
(add-hook! 'evil-org-mode-hook
    (evil-define-key 'normal evil-org-mode-map
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line))
(define-key org-noter-doc-mode-map (kbd "i") 'org-noter-insert-note)
