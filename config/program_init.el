;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIT

(global-set-key (kbd "C-x g") 'magit-status)

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
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

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
         (setq python-shell-interpreter "python"
               python-shell-interpreter-args
               (concat (pe/project-root-function-default)
                       "manage.py shell_plus")
               shell-plus t)
         (message "interpreter: python manage.py shell_plus"))
        ((eq shell-plus t)
         (setq python-shell-interpreter "ipython"
               python-shell-interpreter-args ""
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SQL

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

(setq sql-postgres-login-params
      '((server :default "localhost")
        (port :default 5432)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JAVASCRIPT & NODE

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
;; (add-hook 'js-mode-hook 'js2-minor-mode)
(setq inferior-js-program-command "node")
(add-hook 'js2-mode-hook
          '(lambda ()
             (local-set-key "\C-x\C-e" 'js-send-last-sexp)
             (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
             (local-set-key "\C-cb" 'js-send-buffer)
             (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
             (local-set-key "\C-cl" 'js-load-file-and-go)))
(setenv "NODE_NO_READLINE" "1")
(setq js2-include-node-externs t)
(defun pretty-print-json(&optional b e)
  "Shells out to Python to pretty print JSON"
  (interactive "r")
  (shell-command-on-region b e "python -m json.tool" (current-buffer) t))
;; (add-to-list 'company-backends 'company-tern)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MISC.

;; rest client
(add-to-list 'auto-mode-alist '("\.rest$" . restclient-mode))

;; white space
(setq-default whitespace-line-column 80
              whitespace-style '(face lines-tail trailing))
(add-hook 'prog-mode-hook #'whitespace-mode)

;; pretty symbols
(add-hook 'js2-mode-hook
          (lambda ()
            (push '("function" . ?ƒ) prettify-symbols-alist)))
