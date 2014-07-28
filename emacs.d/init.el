(require 'package)
; list the packages you want
(setq package-list '(cider
                     clojure-mode
                     markdown-mode
                     sass-mode
                     evil
                     evil-leader
                     exec-path-from-shell
                     smartparens
                     flx-ido
                     smex
                     direx
                     projectile
                     rainbow-delimiters
                     magit
                     git-gutter
                     gist
                     golden-ratio
                     noctilux-theme
                     solarized-theme
                     zenburn-theme))

; things to try
; flash sexp-eval: https://github.com/samaaron/nrepl-eval-sexp-fu
; M-x ido:         https://github.com/nonsequitur/smex

; list the repositories containing them
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

; activate all the packages (in particular autoloads)
(package-initialize)

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

; General UI stuff
(global-linum-mode t)
(global-hl-line-mode t)
(setq inhibit-startup-message t)
(setq x-underline-at-descent-line t)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(unless (display-graphic-p) (menu-bar-mode -1))
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 1)

(require 'server)
(unless (server-running-p)
  (server-start))
(setq server-socket-dir (format "/tmp/emacs%d" (user-uid)))

(setq-default show-trailing-whitespace t)
(add-hook 'term-mode-hook (lambda () (setq show-trailing-whitespace nil)))
(setq-default visible-bell 'top-bottom)
(setq-default default-tab-width 2)
(setq-default indent-tabs-mode nil)

(load-theme 'solarized-dark t)
(set-default-font "M+ 1mn-15")

;; See http://www.delorie.com/gnu/docs/elisp-manual-21/elisp_620.html
;; and http://www.gnu.org/software/emacs/manual/elisp.pdf
;; disable line wrap
(setq default-truncate-lines t)
;; make side by side buffers function the same as the main window
(setq truncate-partial-width-windows nil)
;; Add F12 to toggle line wrap
;; TODO REBIND THIS (global-set-key [f12] 'toggle-truncate-lines)

(require 'golden-ratio)

(require 'exec-path-from-shell)
(setq exec-path-from-shell-arguments
  (delete "-i" exec-path-from-shell-arguments))
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(require 'smex)   ; Not needed if you use package.el
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                  ; when Smex is auto-initialized on its first run.
(global-set-key (kbd "M-x") 'smex)

(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching 0)

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1) ;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(require 'recentf)
(recentf-mode 1)

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "." 'eval-buffer
  "r" 'recentf-ido-find-file
  "," 'projectile-find-file
  "t" 'direx:jump-to-directory
  "c" 'comment-or-uncomment-region
  "g" 'golden-ratio
  "w" 'save-buffer
  "b" 'switch-to-buffer
  "k" 'kill-buffer
  ">" 'sp-slurp-hybrid-sexp) ; TODO paredit keybindings

(setq evil-want-C-u-scroll         t
      evil-want-C-w-in-emacs-state t)

(require 'evil)
(evil-mode t)

(defun my-move-key (keymap-from keymap-to key)
     "Moves key binding from one keymap to another, deleting from the old location. "
     (define-key keymap-to key (lookup-key keymap-from key))
     (define-key keymap-from key nil))

(my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(my-move-key evil-motion-state-map evil-normal-state-map " ")
(define-key evil-insert-state-map "k" #'cofi/maybe-exit)

(evil-define-command cofi/maybe-exit ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
    (insert "k")
    (let ((evt (read-event (format "Insert %c to exit insert state" ?k)
                           nil 0.5)))
      (cond
        ((null evt) (message ""))
        ((and (integerp evt) (char-equal evt ?j))
         (delete-char -1)
         (set-buffer-modified-p modified)
         (push 'escape unread-command-events))
        (t (setq unread-command-events (append unread-command-events
                                               (list evt))))))))

(require 'rainbow-delimiters nil)
(global-rainbow-delimiters-mode t)

(require 'icomplete)

(smartparens-global-mode t)
; https://github.com/Fuco1/smartparens/wiki/Example-configuration

(require 'git-gutter)
(git-gutter:linum-setup)
(global-git-gutter-mode +1)
(add-to-list 'git-gutter:update-hooks 'after-save-hook) ; this should be unnecessary
; staging via git-gutter not working?

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
