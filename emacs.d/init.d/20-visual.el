(global-linum-mode -1)
(global-hl-line-mode nil)
(setq inhibit-startup-message t)
(setq x-underline-at-descent-line nil)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(unless (display-graphic-p) (menu-bar-mode -1))

(setq column-number-mode t)

(setq-default show-trailing-whitespace t)
(add-hook 'term-mode-hook
          (lambda () (setq show-trailing-whitespace nil)))
;; (setq-default visible-bell 'top-bottom)

(set-frame-font "M+ 1mn-14")
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

(require 'git-gutter)
(global-git-gutter-mode +1)

;(require 'symon)
;(symon-mode)
