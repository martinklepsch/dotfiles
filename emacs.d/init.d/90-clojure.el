(add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))

;(add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)
(add-hook 'inf-clojure-mode-hook #'clojure-font-lock-setup)
(add-hook 'inf-clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'inf-clojure-mode-hook
          (lambda ()
            (setq-default show-trailing-whitespace nil)))

(require 'rainbow-delimiters nil)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)

(defun boot-dev ()
  (interactive) (inf-clojure "boot dev"))

(defun boot-repl ()
  (interactive) (inf-clojure "boot repl"))

(defun boot-repl-client ()
  (interactive) (inf-clojure "boot repl --client"))

(defun planck ()
  (interactive) (inf-clojure "planck"))

(defun lumo ()
  (interactive) (inf-clojure "lumo -d"))

(defun clj-repl ()
  (interactive) (inf-clojure "clj"))

(defun lein-repl ()
  (interactive) (inf-clojure "lein repl"))

(defun lein-connect ()
  (interactive) (inf-clojure "lein repl :connect"))

(setq inf-clojure-program "boot repl")

(defun system-reset ()
  (interactive)
  (set-buffer "*inf-clojure*")
  (goto-char (point-max))
  (insert "(system.repl/reset)")
  (comint-send-input))

(defun acq-reset ()
  (interactive)
  (set-buffer "*inf-clojure*")
  (goto-char (point-max))
  (insert "(cc.in.server/restart!)")
  (comint-send-input))

;; (defun integrant-reset ()
;;   (interactive)
;;   (set-buffer "*inf-clojure*")
;;   (goto-char (point-max))
;;   (insert "(integrant.repl/reset)")
;;   (comint-send-input))

(defun cider-nrepl-reset ()
  (interactive)
  (save-some-buffers)
  (cider-switch-to-relevant-repl-buffer)
  (insert "(user/reset)")
  (cider-repl-return))

(defun integrant-halt-go ()
  (interactive)
  (cider-interactive-eval "(do (integrant.repl/halt) (integrant.repl/go))"))

(defun integrant-reset ()
  (interactive)
  (cider-interactive-eval "(integrant.repl/reset)"))

(defun spec-instrument ()
  (interactive)
  (cider-interactive-eval "(stest/instrument)"))

;; Fix indentation of some common macros

(require 'clojure-mode)
(define-clojure-indent
  (for-all 1)
  (defroutes 'defun)
  (GET 2)
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
  (context 2)
  (reporting 1))
