;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEY BINDINGS

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
(define-key evil-normal-state-map (kbd ",f") 'projectile-find-file)
(define-key evil-normal-state-map (kbd ",,") 'evil-buffer)
(define-key evil-normal-state-map (kbd ",a") 'helm-ag)
(define-key evil-normal-state-map (kbd ",b") 'helm-mini)
(define-key evil-normal-state-map (kbd ",k") 'kill-buffer)
(define-key evil-normal-state-map (kbd ",r") 'package-refresh-contents)
(define-key evil-normal-state-map (kbd ",p") 'package-list-packages)
(define-key evil-normal-state-map (kbd ",m") 'helm-M-x)
(define-key evil-normal-state-map (kbd "q") nil)
