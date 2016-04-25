;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Emacs 24.5  - init.el                                                      ;;
;; Ogi Martinovic                                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE MANAGEMENT

;; packages sites

;; package.el
(require 'package)
(setq package-archives
      '(
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("melpa" . "http://melpa.org/packages/")
        ))
(setq package-enable-at-startup nil)
(package-initialize)

;; refresh packages
(when (not package-archive-contents)
  (package-refresh-contents))

;; packages
(defvar my-packages
  '(;; evil
    evil evil-leader
    ;; project
    projectile magit
    ;; helm
    helm helm-ag helm-projectile
    ;; completion & syntax
    company flycheck
    ;; common lisp
    slime slime-company
     ;; clojure
    clojure-mode cider
    ;; python
    company-jedi virtualenvwrapper
    ;; web
    markdown-mode restclient
    ;; themes colors faces and modelines
    atom-one-dark-theme smart-mode-line
    highlight-numbers highlight-quoted paren-face
    ;; osx
    exec-path-from-shell))

;; install packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INIT FILES

;; general settings
(load "~/.emacs.d/config/general_init.el")

;; key bindings, helm
(load "~/.emacs.d/config/keybind_init.el")

;; programming (company, git, common lisp, clojure, python, sql, js)
(load "~/.emacs.d/config/program_init.el")

;; org-mode
(load "~/.emacs.d/config/orgmode_init.el")

;; helm, projectile, project explorer
(load "~/.emacs.d/config/project_init.el")

;; key bindings, evil mode
(load "~/.emacs.d/config/keybind_init.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SERVER

;; start emacs server
(server-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EMACS GENERATED CUSTOM SET

