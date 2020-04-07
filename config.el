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
(setq org-capture-templates
      '(("s" "School Todo" entry (file+headline "~/doc/org/todo.org" "Inbox")
         "* TODO %?\n  %a (for school)\n")
        ("p" "Personal Todo" entry (file+headline "~/doc/org/todo.org" "Inbox")
         "* TODO %?\n  %a (for personal)\n")
        ("n" "Note" entry (file+headline "~/doc/org/notes.org" "Notes")
         "* NOTE %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/doc/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-start-day "0d")
(setq org-agenda-span 'day)

(after! mu4e
  (setq! mu4e-get-mail-command "mbsync -a && mu index --maildir=~/.local/share/mail/"
         mu4e-index-update-in-background t
         mu4e-use-fancy-chars t
         mu4e-compose-signature-auto-include t
         mu4e-use-fancy-chars t
         mu4e-view-show-addresses t
         mu4e-view-show-images t
         mu4e-compose-format-flowed t
         mu4e-maildir "~/.local/share/mail"
         ;mu4e-compose-in-new-frame t
         mu4e-change-filenames-when-moving t ;; http://pragmaticemacs.com/emacs/fixing-duplicate-uid-errors-when-using-mbsync-and-mu4e/
)
  ;; Add an open in browser action
  (add-to-list 'mu4e-headers-actions
             '("in browser" . mu4e-action-view-in-browser) t)
  (add-to-list 'mu4e-view-actions
             '("in browser" . mu4e-action-view-in-browser) t)
  (setq! mu4e-compose-context-policy 'ask-if-none
         mu4e-context-policy 'pick-first
         mu4e-contexts
            `( ,(make-mu4e-context
                    :name "gmail"
                    :match-func (lambda (msg)
                                    (when msg
                                        (mu4e-message-contact-field-matches msg
                                        :to "alexday135@gmail.com")))
                    :enter-func '()
                    :leave-func (lambda () (mu4e-clear-caches))
                    :vars '(
                            (user-mail-address      . "alexday135@gmail")
                            (user-full-name         . "Alex Day")
                            (mu4e-drafts-folder     . "/gmail/Drafts")
                            (mu4e-refile-folder     . "/gmail/Archive")
                            (mu4e-sent-folder       . "/gmail/Sent Mail")
                            (mu4e-trash-folder      . "/gmail/Trash")
                            (mu4e-update-interval   . 1800)))
                ,(make-mu4e-context
                    :name "alexday"
                    :match-func (lambda (msg)
                                    (when msg
                                        (mu4e-message-contact-field-matches msg
                                            :to "alex@alexday.me")))
                    :enter-func '()
                    :leave-func (lambda () (mu4e-clear-caches))
                    :vars '((user-mail-address      . "alex@alexday.me")
                            (user-full-name         . "Alex Day")
                            (mu4e-drafts-folder     . "/alexdayme/Drafts")
                            (mu4e-refile-folder     . "/alexdayme/Archive")
                            (mu4e-sent-folder       . "/alexdayme/Sent")
                            (mu4e-trash-folder      . "/alexdayme/Trash")
                            (mu4e-update-interval   . 1800)))))
(setq! sendmail-program "/usr/bin/msmtp"
       message-sendmail-f-is-evil t
       message-sendmail-extra-arguments '("--read-envelope-from")
       message-send-mail-function 'message-send-mail-with-sendmail)
  (setq! message-citation-line-format "On %a %d %b %Y at %R, %f wrote:\n"
         message-citation-line-function 'message-insert-formatted-citation-line
         message-kill-buffer-on-exit t)
)
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
