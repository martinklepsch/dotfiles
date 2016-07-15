(require 'package)
(package-initialize)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

; list the packages you want
(setq package-list
      '(use-package
         ;; OCaml
         merlin tuareg utop
         ;; Lisp + Clojure tooling
         inf-clojure smartparens ; cider geiser
         ;; lang-modes
         clojure-mode markdown-mode php-mode sass-mode scss-mode lua-mode fish-mode
         yaml-mode applescript-mode racket-mode
         ;; Evil
         evil evil-org evil-leader
         ;; Navigating
         expand-region direx flx-ido helm helm-ag helm-projectile projectile
         ;; Git & Github
         magit gist
         ;; Utils
         symon exec-path-from-shell key-chord ;company
         ;; Visuals
         git-gutter rainbow-delimiters golden-ratio sublime-themes
         grandshell-theme cyberpunk-theme tronesque-theme
         noctilux-theme solarized-theme zenburn-theme
         color-theme-sanityinc-tomorrow gruvbox-theme))

(setq use-package-always-ensure t)
(require 'use-package)
(use-package nix-mode
  :mode "\\.nix\\'")

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
