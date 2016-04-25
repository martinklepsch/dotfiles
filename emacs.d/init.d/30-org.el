(setq org-agenda-files (quote ("~/Dropbox/org")))
(setq org-startup-indented t)
(setq org-src-fontify-natively t)
(setq org-default-notes-file "~/testing.org");(concat org-directory "/notes.org"))
(setq org-capture-templates
      '(("p" "Day Plan" entry (file+datetree "~/Dropbox/org/journal.org")
         "* Today :dayplan:\n%?")
        ("j" "Journal Entry" entry (file+datetree "~/Dropbox/org/journal.org")
         "* %<%H:%M> %?")
        ("t" "Todo" entry (file+headline "~/Dropbox/org/testing.org" "Tasks")
         "* TODO %?")))

(setq org-todo-keywords
       '((sequence "TODO(t!)" "NEXT(n!)" "|" "DONE(d!)" "CANCELED(c@/!)")))

(setq org-todo-keyword-faces
      '(("NEXT" . (:foreground "yellow" :weight bold))))

(setq org-refile-targets '((org-agenda-files :tag . "refile")))

(add-hook 'org-agenda-mode-hook
          (lambda ()
            ;; racket-run
            (define-key org-agenda-mode-map (kbd "j") 'org-agenda-next-item)
            (define-key org-agenda-mode-map (kbd "k") 'org-agenda-previous-item)))
