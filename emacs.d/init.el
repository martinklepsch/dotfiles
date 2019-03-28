;; Directories and file names

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(setq mk-emacs-init-file
      (or load-file-name buffer-file-name))
(setq mk-emacs-config-dir
      (file-name-directory mk-emacs-init-file))
(setq user-emacs-directory mk-emacs-config-dir)
(setq mk-elisp-dir
      (expand-file-name "elisp" mk-emacs-config-dir))
(setq mk-elisp-external-dir
      (expand-file-name "external" mk-elisp-dir))
(setq mk-themes-dir
      (expand-file-name "themes" mk-elisp-dir))
(setq mk-init-dir
      (expand-file-name "init.d" mk-emacs-config-dir))

;; Load all elisp files in ./init.d
(if (file-exists-p mk-init-dir)
    (dolist (file (directory-files mk-init-dir t "\\.el$"))
      (load file)))

(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'zprint-mode)

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name (concat mk-emacs-config-dir "bak")))))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(compilation-message-face (quote default))
 '(css-indent-offset 2)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("0e0c37ee89f0213ce31205e9ae8bce1f93c9bcd81b1bcda0233061bb02c357a8" "086970da368bb95e42fd4ddac3149e84ce5f165e90dfc6ce6baceae30cf581ef" "f2057733672d3b119791f5b7d1a778bf8880121f22ea122a21d221b45081f49e" "15348febfa2266c4def59a08ef2846f6032c0797f001d7b9148f30ace0d08bcf" "0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" "442c946bc5c40902e11b0a56bd12edc4d00d7e1c982233545979968e02deb2bc" "0c311fb22e6197daba9123f43da98f273d2bfaeeaeb653007ad1ee77f0003037" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "f0a99f53cbf7b004ba0c1760aa14fd70f2eabafe4e62a2b3cf5cabae8203113b" "ee6081af57dd389d9c94be45d49cf75d7d737c4a78970325165c7d8cb6eb9e34" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(fill-column 95)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(ledger-reports
   (quote
    ((nil "ledger ")
     ("bal" "%(binary) -f %(ledger-file) bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)"))))
 '(magit-diff-use-overlays nil)
 '(magit-use-overlays nil)
 '(org-agenda-files
   (quote
    ("/Users/martin/Dropbox/org/journal.org" "/Users/martin/Dropbox/org/testing.org")))
 '(package-selected-packages
   (quote
    (fennel-mode flycheck go-mode dockerfile-mode inf-clojure cider systemd nginx-mode adoc-mode swift-mode tao-theme direnv terraform-mode clj-refactor graphql-mode zenburn-theme yaml-mode utop use-package tuareg tronesque-theme symon sublime-themes solarized-theme smartparens scss-mode sass-mode rainbow-delimiters racket-mode php-mode parinfer noctilux-theme nix-mode merlin markdown-mode magit lua-mode key-chord helm-projectile helm-ag gruvbox-theme grandshell-theme golden-ratio git-gutter gist flx-ido fish-mode expand-region exec-path-from-shell evil-org direx cyberpunk-theme color-theme-sanityinc-tomorrow applescript-mode)))
 '(safe-local-variable-values
   (quote
    ((cider-clojure-cli-global-options . "-O:lambdawerk")
     (cider-cljs-lein-repl . "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))"))))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
