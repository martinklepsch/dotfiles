(global-set-key (kbd "M-x") 'helm-M-x)

(global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)
(setq evil-want-C-w-in-emacs-state t)
(setq evil-want-C-u-scroll t)
(setq evil-want-C-i-jump nil)
(setq evil-toggle-key "C-`") ;; never need this, don't know how to unbind

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "." 'eval-buffer
  "a" 'helm-projectile-ag
  "," 'helm-projectile-find-file
  "p s" 'helm-projectile-switch-project
  "p i c" 'projectile-invalidate-cache
  "t" 'direx-project:jump-to-project-root
  "c" 'comment-or-uncomment-region
  "i" 'spec-instrument
  "l" 'cider-jump-to-var
  "g" 'golden-ratio-mode
  "w" 'save-buffer
  "b" 'helm-mini
  "k" 'kill-buffer
  ">" 'sp-slurp-hybrid-sexp) ; TODO paredit keybindings

(require 'evil)
(evil-mode t)

(evil-declare-key 'normal direx:direx-mode-map (kbd "r")   'direx:refresh-whole-tree)
(evil-declare-key 'normal direx:direx-mode-map (kbd "o")   'direx:find-item-other-window)
(evil-declare-key 'normal direx:direx-mode-map (kbd "f")   'direx:find-item)
(evil-declare-key 'normal direx:direx-mode-map (kbd "RET") 'direx:maybe-find-item)
(evil-declare-key 'normal direx:direx-mode-map (kbd "V")   'direx:view-item-other-window)
(evil-declare-key 'normal direx:direx-mode-map (kbd "v")   'direx:view-item)
(evil-declare-key 'normal direx:direx-mode-map (kbd "g")   'direx:refresh-whole-tree)
(evil-declare-key 'normal direx:file-map       (kbd "+")   'direx:create-directory)

(global-set-key (kbd "C-k") (lambda () (interactive) (evil-previous-line 10)))
(global-set-key (kbd "C-j") (lambda () (interactive) (evil-next-line 10)))

(require 'key-chord)
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
;; (key-chord-define-global "" 'helm-mini)
;; (key-chord-define-global "cqc" 'inf-clojure-eval-last-sexp)
