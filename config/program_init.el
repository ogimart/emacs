;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIT

(global-set-key (kbd "C-x g") 'magit-status)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPANY

;; global company-mode
(add-hook 'after-init-hook 'global-company-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLOJURE

;; start clojure repl key bind <C-c j>
(global-set-key (kbd "C-c j") 'cider-jack-in)

;; enamble eldoc mode
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; bind <RET> to auto-indent <C-j>
(add-hook 'clojure-mode-hook
  '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))

;; syntax color in repl
(setq cider-repl-use-clojure-font-lock t)
(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)

;; associate clojurescript with clojure mode
(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))

;; clojure completion
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PYTHON

;; virtual env
(venv-initialize-interactive-shells)
(venv-initialize-eshell)
(setq venv-location "~/.virtualenvs")
(setq-default mode-line-format
              (cons '(:exec venv-current-name) mode-line-format))

;; ipython
(setq python-shell-interpreter "ipython")

;; ipdb
(defun ipdb:insert-trace (arg)
  (interactive "p")
  (open-previous-line arg)
  (insert "import ipdb; ipdb.set_trace()"))
(add-hook 'python-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c C-t") 'ipdb:insert-trace)))
(defun ipdb:highlight-trace ()
  (interactive)
  (highlight-lines-matching-regexp "import ipdb; ipdb.set_trace()"))
(add-hook 'python-mode-hook 'ipdb:highlight-trace)

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
             (local-set-key (kbd "C-.") 'jedi:jump-to-definition)
             (local-set-key (kbd "C-,") 'jedi:jump-back)
             (local-set-key (kbd "C-c d") 'jedi:show-doc)))

;; code check
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
;; COMMON LISP

(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq slime-contribs '(slime-fancy))
(slime-setup '(slime-company))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MISC.

;; rainbow parenthesis
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; white space
(setq-default whitespace-line-column 80
              whitespace-style '(face lines-tail trailing))
(add-hook 'prog-mode-hook #'whitespace-mode)

;; pretty symbols
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (push '("lambda" . 955) prettify-symbols-alist)))
(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("\\(#\\)("
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "λ") nil))))))
(add-hook 'python-mode-hook
          (lambda ()
            (push '("lambda" . 955) prettify-symbols-alist)))
(add-hook 'js2-mode-hook
          (lambda ()
            (push '("function" . ?ƒ) prettify-symbols-alist)))
(global-prettify-symbols-mode t)
