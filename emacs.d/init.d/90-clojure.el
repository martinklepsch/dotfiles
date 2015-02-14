(add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))

(add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)
(add-hook 'inf-clojure-mode-hook #'clojure-font-lock-setup)
(add-hook 'inf-clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'inf-clojure-mode-hook
          (lambda ()
            (setq-default show-trailing-whitespace nil)))

(require 'rainbow-delimiters nil)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)

;; not using cider anymore...
(defun cider-nrepl-reset ()
  (interactive)
  (save-some-buffers)
  (cider-switch-to-relevant-repl-buffer)
  (insert "(user/reset)")
  (cider-repl-return))

(setq inf-clojure-program "boot repl")
