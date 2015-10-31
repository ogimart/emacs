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
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(setq package-enable-at-startup nil)
(package-initialize)

;; refresh packages
(when (not package-archive-contents)
  (package-refresh-contents))

;; packages
(defvar my-packages
  '(;; helm
    helm helm-ag helm-projectile
    ;; git
    magit
    ;; completion & syntax
    company flycheck
    ;; project
    projectile project-explorer
    ;; clojure
    clojure-mode cider
    ;; python
    company-jedi virtualenvwrapper
    ;; lisp
    slime slime-company
    ;; javascript
    js2-mode js-comint ;tern company-tern
    ;; web
    restclient
    ;; themes
    noctilux-theme rainbow-delimiters
    ;; osx
    exec-path-from-shell))

;; install packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INIT FILES

;; general and look settings
(load "~/.emacs.d/config/general_init.el")

;; key bindings, helm
(load "~/.emacs.d/config/keybind_init.el")

;; programming (company, git, clojure, python, lisp, sql)
(load "~/.emacs.d/config/program_init.el")

;; org-mode
(load "~/.emacs.d/config/orgmode_init.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SERVER

;; start emacs server
(server-start)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EMACS GENERATED CUSTOM SET


