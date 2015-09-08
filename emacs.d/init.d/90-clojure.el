(add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))

(add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)
(add-hook 'inf-clojure-mode-hook #'clojure-font-lock-setup)
(add-hook 'inf-clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'inf-clojure-mode-hook
          (lambda ()
            (setq-default show-trailing-whitespace nil)))

(require 'rainbow-delimiters nil)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)

(defun cider-nrepl-reset ()
  (interactive)
  (save-some-buffers)
  (cider-switch-to-relevant-repl-buffer)
  (insert "(user/reset)")
  (cider-repl-return))

(defun boot-repl ()
  (interactive) (inf-clojure "boot repl"))

(defun boot-repl-client ()
  (interactive) (inf-clojure "boot repl --client"))

(defun lein-repl ()
  (interactive) (inf-clojure "lein repl"))

(setq inf-clojure-program "boot repl")
