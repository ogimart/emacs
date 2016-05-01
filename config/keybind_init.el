;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEY BINDINGS EMACS MODE

;; join the current line with the line beneath it
(defun top-join-line ()
  (interactive)
  (delete-indentation 1))
(global-set-key (kbd "C-^") 'top-join-line)

;; clipboard
(global-set-key (kbd "A-v") 'clipboard-yank)

;; Behave like vi's o command
(defun open-next-line (arg)
  "Move to the next line and then opens a line."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))
(global-set-key (kbd "C-o") 'open-next-line)

;; Behave like vi's O command
(defun open-previous-line (arg)
  "Open a new line before the current."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))
(global-set-key (kbd "M-o") 'open-previous-line)

;; Autoindent open-*-lines
(defvar newline-and-indent t)

;; move line up
(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (previous-line 2))
(global-set-key (kbd "M-p") 'move-line-up)

;; move line down
(defun move-line-down ()
  (interactive)
  (next-line 1)
  (transpose-lines 1)
  (previous-line 1))
(global-set-key (kbd "M-n") 'move-line-down)

;; switch buffers
(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EVIL MODE

(require 'evil)
(evil-mode 1)

;; normal mode
(define-key evil-normal-state-map (kbd "<SPC>e") 'find-file)
(define-key evil-normal-state-map (kbd "<SPC>k") 'kill-buffer)
(define-key evil-normal-state-map (kbd "<SPC>s") 'eshell)
(define-key evil-normal-state-map (kbd "<SPC>[") 'previous-buffer)
(define-key evil-normal-state-map (kbd "<SPC>]") 'next-buffer)
(define-key evil-normal-state-map (kbd "<SPC>f") 'projectile-find-file)
(define-key evil-normal-state-map (kbd "<SPC>x") 'projectile-kill-buffers)
(define-key evil-normal-state-map (kbd "<SPC>p") 'projectile-switch-project)
(define-key evil-normal-state-map (kbd "<SPC>o") 'project-explorer-toggle)
(define-key evil-normal-state-map (kbd "<SPC>m") 'helm-M-x)
(define-key evil-normal-state-map (kbd "<SPC>a") 'helm-ag)
(define-key evil-normal-state-map (kbd "<SPC>b") 'helm-mini)
(define-key evil-normal-state-map (kbd "<SPC>q") 'sql:server-connect)
;(define-key evil-normal-state-map (kbd "q") nil)

;; cl-evil-mode

;; clj-evil-mode
(define-minor-mode clj-evil-mode
  "Clojure Evil minor mode"
  :keymap (make-sparse-keymap))

(with-eval-after-load "~/.emacs.d/config/program_init.el"
  (evil-define-key 'normal clj-evil-mode-map ",j" #'cider-jack-in)
  (evil-define-key 'normal clj-evil-mode-map ",e" #'cider-eval-last-sexp)
  (evil-define-key 'normal clj-evil-mode-map ",x" #'cider-eval-defun-at-point)
  (evil-define-key 'normal clj-evil-mode-map ",r" #'cider-eval-region)
  (evil-define-key 'normal clj-evil-mode-map ",k" #'cider-eval-buffer)
  (evil-define-key 'normal clj-evil-mode-map ",s" #'cider-eval-ns-form)
  (evil-define-key 'normal clj-evil-mode-map ",l" #'cider-load-file)
  (evil-define-key 'normal clj-evil-mode-map ",n" #'cider-repl-set-ns)
  (evil-define-key 'normal clj-evil-mode-map ",." #'cider-find-var)
  (evil-define-key 'normal clj-evil-mode-map ",," #'cider-jump-back)
  (evil-define-key 'normal clj-evil-mode-map ",z" #'cider-switch-to-repl-buffer)
  (add-hook 'clojure-mode-hook 'clj-evil-mode))

;; py-evil-mode
(define-minor-mode py-evil-mode
  "Python Evil minor mode"
  :keymap (make-sparse-keymap))

(with-eval-after-load "~/.emacs.d/config/program_init.el"
  (evil-define-key 'normal py-evil-mode-map ",g" #'shell-plus-toggle)
  (evil-define-key 'normal py-evil-mode-map ",t" #'ipdb:insert-trace)
  (evil-define-key 'normal py-evil-mode-map ",d" #'jedi:jump-to-definition)
  (evil-define-key 'normal py-evil-mode-map ",b" #'jedi:jump-back)
  (evil-define-key 'normal py-evil-mode-map ",k" #'jedi:show-doc)
  (add-hook 'python-mode-hook 'py-evil-mode))
