;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPANY

;; global company-mode
(add-hook 'after-init-hook 'global-company-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMON LISP

;; slime
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq slime-contribs '(slime-fancy))
(slime-setup '(slime-company))

;; paren face mode, highlight numbers and quote
(add-hook 'lisp-mode-hook 'highlight-numbers-mode)
(add-hook 'lisp-mode-hook 'highlight-quoted-mode)
(add-hook 'lisp-mode-hook 'paren-face-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLOJURE

;; start clojure repl key bind <C-c j>
(global-set-key (kbd "C-c j") 'cider-jack-in)

;; enable eldoc mode
(add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)

;; refactoring
(add-hook 'clojure-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c C-r") 'cljr-helm)))

;; bind <RET> to auto-indent <C-j>
(add-hook 'clojure-mode-hook
  '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))

;; paren face mode, highlight numbers and quote
(add-hook 'clojure-mode-hook 'highlight-numbers-mode)
(add-hook 'clojure-mode-hook 'highlight-quoted-mode)
(add-hook 'clojure-mode-hook 'paren-face-mode)

;; syntax color in repl
(setq cider-repl-use-clojure-font-lock t)
;(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)

;; associate clojurescript with clojure mode
(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))

;; clojure completion
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)

;; boot
(add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PYTHON

;; virtual env
(venv-initialize-interactive-shells)
(venv-initialize-eshell)
(setq venv-location "~/.virtualenvs")
(setq-default mode-line-format
              (cons '(:exec venv-current-name) mode-line-format))

;; interpeter: ipython / django shell_plus
(setq python-shell-interpreter "ipython")
(defvar shell-plus nil)
(defun shell-plus-toggle ()
  (interactive)
  (cond ((eq shell-plus nil)
         (setq python-shell-interpreter-args
               (concat (projectile-project-root)
                       "manage.py shell_plus")
               shell-plus t)
         (message "interpreter: ipython manage.py shell_plus"))
        ((eq shell-plus t)
         (setq python-shell-interpreter-args ""
               shell-plus nil)
         (message "interpreter: ipython"))))

;; ipdb
(defun ipdb:insert-trace (arg)
  (interactive "p")
  (open-previous-line arg)
  (insert "import ipdb; ipdb.set_trace()")
  (highlight-lines-matching-regexp "^[ ]*import ipdb; ipdb.set_trace()"))

;; python completion
(add-hook 'python-mode-hook
          (lambda ()
            (add-to-list 'company-backends 'company-jedi)))
(setq jedi:complete-on-dot t)

;; jedi
(defvar jedi:goto-stack '())
(defun jedi:jump-to-definition ()
  (interactive)
  (add-to-list 'jedi:goto-stack
               (list (buffer-name) (point)))
  (jedi:goto-definition))
(defun jedi:jump-back ()
  (interactive)
  (let ((p (pop jedi:goto-stack)))
    (if p (progn
            (switch-to-buffer (nth 0 p))
            (goto-char (nth 1 p))))))
(add-hook 'python-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c C-g") 'shell-plus-toggle)
             (local-set-key (kbd "C-c C-t") 'ipdb:insert-trace)
             (local-set-key (kbd "C-c C-d") 'jedi:jump-to-definition)
             (local-set-key (kbd "C-c C-b") 'jedi:jump-back)
             (local-set-key (kbd "C-c C-k") 'jedi:show-doc)))

;; Code check
(add-hook 'python-mode-hook 'flycheck-mode)

;; tabs
(add-hook 'python-mode-hook
      (lambda ()
        (setq indent-tabs-mode nil)
        (setq tab-width 4)
        (setq python-indent 4)))

(add-hook 'python-mode-hook 'turn-on-eldoc-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SQL
;;
;; The sql-connection.el file should have the folllowing:
;;
;; (setq sql-connection-alist
;;       '((pg-local-conn (sql-product 'postgres)
;;                        (sql-port 5432)
;;                        (sql-server "localhost")
;;                        (sql-user "user")
;;                        (sql-password "pass")
;;                        (sql-database "db"))))
;;
;; (defun sql:pg-local-server ()
;;   (interactive)
;;   (setq sql-product 'postgres)
;;   (sql-connect 'pg-local-conn))
;;
;; (defvar sql:servers-list
;;   '(("local lms" sql:pg-local-server)))
;;
;; Postgres specific: create ~/.pgpass file in format:
;; host:port:db:user:password

(load "~/.emacs.d/config/sql-connection.el")

(defun sql:server-connect (func)
  (interactive (helm-comp-read "Select server: " sql:servers-list))
  (funcall func))

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MISC.

;; rest client
(add-to-list 'auto-mode-alist '("\.rest$" . restclient-mode))

;; white space
(setq-default whitespace-line-column 80
              whitespace-style '(face lines-tail trailing))
(add-hook 'prog-mode-hook #'whitespace-mode)
