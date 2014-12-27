(require 'package)
(package-initialize)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

; list the packages you want
(setq package-list '(cider
                     company
                     geiser ;racket
                     lua-mode
                     clojure-mode
                     markdown-mode
                     php-mode
                     sass-mode
                     key-chord
                     evil
                     evil-org
                     evil-leader
                     exec-path-from-shell
                     smartparens
                     flx-ido
                     helm
                     helm-ag
                     helm-projectile
                     direx
                     projectile
                     rainbow-delimiters
                     magit
                     git-gutter
                     gist
                     golden-ratio
                     sublime-themes
                     grandshell-theme
                     cyberpunk-theme
                     tronesque-theme
                     noctilux-theme
                     solarized-theme
                     zenburn-theme
                     color-theme-sanityinc-tomorrow))

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


;; Always ALWAYS use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(load-library "iso-transl")

;; Automatically save buffers before compiling
(setq compilation-ask-about-save nil)

;; Always ask for y/n keypress instead of typing out 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name (concat dotfiles-dir "bak")))))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

; General UI stuff
(global-linum-mode -1)
(global-hl-line-mode t)
(setq inhibit-startup-message t)
(setq x-underline-at-descent-line t)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(unless (display-graphic-p) (menu-bar-mode -1))
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
; make em-dashes and tilde stuff work
(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)

(setq scroll-step 1)
(setq column-number-mode t)
(setq ns-pop-up-frames nil)
(global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)

(add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))

(require 'server)
(unless (server-running-p)
  (server-start))

(setq-default show-trailing-whitespace t)
(add-hook 'term-mode-hook (lambda () (setq show-trailing-whitespace nil)))
(setq-default visible-bell 'top-bottom)
(setq-default default-tab-width 2)
(setq-default indent-tabs-mode nil)

(set-frame-font "M+ 1mn-15")
(setq solarized-use-variable-pitch nil)
(setq solarized-height-minus-1 1.0)
(setq solarized-height-plus-1 1.0)
(setq solarized-height-plus-2 1.0)
(setq solarized-height-plus-3 1.0)
(setq solarized-height-plus-4 1.0)
(load-theme 'solarized-dark t)

;; See http://www.delorie.com/gnu/docs/elisp-manual-21/elisp_620.html
;; and http://www.gnu.org/software/emacs/manual/elisp.pdf
;; disable line wrap
(setq default-truncate-lines t)
;; make side by side buffers function the same as the main window
(setq truncate-partial-width-windows nil)
;; Add F12 to toggle line wrap
;; TODO REBIND THIS (global-set-key [f12] 'toggle-truncate-lines)

(require 'golden-ratio)

(require 'exec-path-from-shell) ;deprecated?
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(require 'helm-config)
(helm-mode 1)
(setq helm-quick-update                     t ; do not display invisible candidates
      helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
      helm-ff-file-name-history-use-recentf t)


(global-set-key (kbd "M-x") 'helm-M-x)

(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-indexing-method 'alien)
(setq projectile-completon-system 'helm)
(require 'helm-projectile)
(helm-projectile-on)

(require 'helm-ag)
(defun helm-projectile-ag ()
  (interactive)
  (helm-do-ag (projectile-project-root)))


(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1) ;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(setq evil-want-C-w-in-emacs-state t)
(setq evil-want-C-u-scroll t)
(setq evil-want-C-i-jump nil)

(defun nrepl-reset ()
  (interactive)
  (save-some-buffers)
  (cider-switch-to-relevant-repl-buffer)
  (insert "(user/reset)")
  (cider-repl-return))

(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; (global-set-key (kbd "C-6") 'switch-to-previous-buffer)

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "." 'eval-buffer
  "r" 'nrepl-reset
  "a" 'helm-projectile-ag
  "," 'helm-projectile-find-file
  "p" 'helm-projectile-switch-project
  "t" 'direx-project:jump-to-project-root
  "c" 'comment-or-uncomment-region
  "x" 'org-capture
  "l" 'cider-jump-to-var
  "g" 'golden-ratio
  "w" 'save-buffer
  "b" 'helm-mini
  "k" 'kill-buffer
  ">" 'sp-slurp-hybrid-sexp) ; TODO paredit keybindings

(require 'evil)
(evil-mode t)

(require 'evil-org)

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

(add-hook 'after-init-hook 'global-company-mode)

(require 'key-chord)
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-insert-state-map "kj" 'evil-normal-state)

(require 'rainbow-delimiters nil)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

(require 'icomplete)

(smartparens-global-mode t)
(sp-pair "'" nil :actions :rem)
; https://github.com/Fuco1/smartparens/wiki/Example-configuration

(require 'git-gutter)
(global-git-gutter-mode +1)

(setq org-startup-indented t)
(setq org-default-notes-file "~/testing.org");(concat org-directory "/notes.org"))
(setq org-capture-templates
  '(("j" "Journal Entry"
         entry (file+datetree "~/journal.org")
         "* %?")
    ("t" "Todo"
         entry (file+headline "~/gtd.org" "Tasks")
         "* TODO %?")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" "442c946bc5c40902e11b0a56bd12edc4d00d7e1c982233545979968e02deb2bc" "0c311fb22e6197daba9123f43da98f273d2bfaeeaeb653007ad1ee77f0003037" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "f0a99f53cbf7b004ba0c1760aa14fd70f2eabafe4e62a2b3cf5cabae8203113b" "ee6081af57dd389d9c94be45d49cf75d7d737c4a78970325165c7d8cb6eb9e34" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
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
 '(magit-diff-use-overlays nil)
 '(magit-use-overlays nil)
 '(org-agenda-files (quote ("~/journal.org" "~/testing.org")))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(tool-bar-mode nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
