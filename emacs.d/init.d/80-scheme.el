(setq scheme-program-name "racket -il xrepl")

(add-hook 'racket-mode-hook
          (lambda ()
            ;; racket-run
            (define-key racket-mode-map (kbd "C-x C-e") 'racket-send-last-sexp)
            (define-key racket-mode-map (kbd "C-c C-c") 'racket-send-definition)
            ;; run-scheme
            ;; (define-key racket-mode-map (kbd "C-x C-e") 'scheme-send-last-sexp)
            ;; (define-key racket-mode-map (kbd "C-c C-c") 'scheme-send-definition)
            ))

(add-hook 'racket-repl-mode-hook
          (lambda ()
            ;(message "Inserting bindings")
            (define-key racket-repl-mode-map (kbd "C-w") nil)
            (define-key racket-repl-mode-map (kbd "C-w C-w") 'evil-window-next)))
