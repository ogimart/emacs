;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OS X

;; (setq mac-command-modifier 'meta)             ; <M->
;; (setq mac-option-modifier 'alt)               ; <A->
;; (setq mac-right-command-modifier 'super)      ; <s->
;; (setq mac-right-option-modifier 'hyper)       ; <H->

;; mac modifier keys - make Command as Meta, Option as Alt
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'alt))

;; homebrew
(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL

;; take the short answer y/n is yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; tabs (spaces only)
(setq-default indent-tabs-mode nil)

;; line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; use ibuffer for list buffers
(defalias 'list-buffers 'ibuffer)

;; show matching parentheses
(show-paren-mode 1)
(defvar show-paren-delay 0)

;; interactively do things
(require 'ido)
(ido-mode t)
(require 'icomplete)

;; follow symbolic links
(setq vc-follow-symlinks t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOOK

;; turn off: tool bar, scroll bar, menu bar and splash screen
(tool-bar-mode 0)
(scroll-bar-mode 0)
;(menu-bar-mode 0)
(setq inhibit-splash-screen t)

;; colors
(setq ns-use-srgb-colorspace t)
(load-theme 'noctilux t)

;; paren match
(set-face-background 'show-paren-match (face-background 'default))
(set-face-background 'show-paren-match "#2f4f4f")
(set-face-attribute 'show-paren-match-face nil
                    :weight 'bold :underline nil)

;; font
(defun set-laptop-font ()
  (interactive)
  (set-default-font "Consolas-11"))
(defun set-monitor-font ()
  (interactive)
  (set-default-font "Consolas-13"))
(set-default-font () (set-laptop-font))

;; default frame size
(setq default-frame-alist '(
  (width . 120)
  (height . 48)))

;; visual bell
(defun my-visible-bell ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))
(setq visible-bell nil
      ring-bell-function 'my-visible-bell)

;; vertical border
(set-face-attribute 'vertical-border
                    nil
                    :foreground "grey13")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BACKUP FILES

(defvar --backup-dir (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-dir))
        (make-directory --backup-dir t))
(setq backup-directory-alist `(("." . ,--backup-dir)))
(setq make-backup-files t          ; backup of a file the first time it is saved
      backup-by-copying t          ; copy
      version-control t            ; version numbers for backup files
      delete-old-versions t        ; delete excess backup files silently
      delete-by-moving-to-trash t  ; move to trash
      auto-save-default t          ; auto-save every buffer that visits a file
      )
