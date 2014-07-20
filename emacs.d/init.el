(require 'package)
; list the packages you want
(setq package-list '(cider
                     clojure-mode
                     markdown-mode
                     evil
                     evil-leader
                     exec-path-from-shell
                     smartparens
                     flx-ido
                     projectile
                     rainbow-delimiters
                     magit
                     git-gutter
                     gist
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

; General UI stuff
(global-linum-mode t)
(global-hl-line-mode t)
(setq default-tab-width 2)
(setq inhibit-startup-message t)
(setq visible-bell 'top-bottom)
(setq x-underline-at-descent-line t)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(unless (display-graphic-p) (menu-bar-mode -1))
(load-theme 'solarized-dark t)
(set-default-font "Monaco-13")

; Make sure $PATH is the one in user shell
; (file-name-nondirectory (getenv "SHELL"))
(require 'exec-path-from-shell)
(setq exec-path-from-shell-arguments (delete "-i" exec-path-from-shell-arguments))
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

; (defun -interpose (sep list)
;   "Returns a new list of all elements in LIST separated by SEP."
;   (let (result)
;     (when list
;       (setq result (cons (car list) result))
;       (setq list (cdr list)))
;     (while list
;       (setq result (cons (car list) (cons sep result)))
;       (setq list (cdr list)))
;     (nreverse result)))

; (when (eq system-type 'darwin)
;   (exec-path-from-shell-initialize)
;   (when (equal (file-name-nondirectory (getenv "SHELL")) "fish")
;     (setq exec-path (split-string (car exec-path) " "))
;     (let ((fixed-path (apply 'concat (-interpose ":" (split-string (getenv "PATH") " ")))))
;          (setenv "PATH" fixed-path)
;          (setq eshell-path-env fixed-path))))

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 1)

(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching 0)

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
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

;; Making Emacs better
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "r" 'recentf-ido-find-file
  "," 'projectile-find-file
  "c" 'comment-or-uncomment-region
  "w" 'save-buffer
  "b" 'switch-to-buffer
  "k" 'kill-buffer)


(setq evil-want-C-u-scroll t
      evil-want-C-w-in-emacs-state t)

(require 'evil)
(evil-mode t)

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
(rainbow-delimiters-mode 1)

(require 'icomplete)

(smartparens-global-mode t)
; https://github.com/Fuco1/smartparens/wiki/Example-configuration

(require 'git-gutter)
(global-git-gutter-mode t)

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
