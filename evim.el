;;; evim.el --- VIM keybindings using native emacs functions and style  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Sam

;; Author: Sam <scmorris.dev@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(add-to-list 'load-path (file-name-directory (f-this-file)))

(require 'untermin)
(require 'dash)
(require 'cl-macs)
(require 'cl-extra)
(require 'state)
(require 'skey)
(require 'evim-lib)

(defmacro evim-define-mode (name)
  `(progn
     (define-minor-mode ,(intern (concat "evim-" (symbol-name name) "-mode"))
       ,(concat "Toggle Evim " (symbol-name name) " minor mode.

Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.")
       :init-value nil
       :lighter ,(concat " Evim:" (symbol-name name))
       :keymap (make-sparse-keymap)
       :group 'evim)))

(defmacro evim-define-derived-mode (child parent)
  `(progn
     (define-minor-mode ,(intern (concat "evim-" (symbol-name parent) "-" (symbol-name child) "-mode"))
       ,(concat "Toggle Evim " (symbol-name child) " derived from " (symbol-name parent) " minor mode.

Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.")
       :lighter ,(concat " Evim:" (symbol-name parent) ":" (symbol-name child))
       :keymap (let ((map (make-sparse-keymap)))
                 (set-keymap-parent map ,(intern (concat "evim-" (symbol-name parent) "-mode-map")))
                 map)
       :group 'evim)))

(evim-define-mode insert)
(skey-define-keys
 '(evim-insert-mode-map)
 '(
   (";" (lambda () (interactive) (insert ";")))
   ("C-a" beginning-of-line)
   ("C-e" end-of-line)
   ("C-f" forward-char)
   ("C-b" backward-char)
   ("C-l" completion-at-point)
   ("C-n" next-line)
   ("C-p" previous-line)
   ("C-s" isearch-forward)
   ("C-v" yank)
   ("C-h" xah-delete-backward-char-or-bracket-text)
   ("<return>" newline-and-indent)
   ("<C-[>" (lambda () (interactive) (state-transition 'evim-insert-mode 'evim-normal-mode))))
 )

(evim-define-mode normal)

(define-key evim-normal-mode-map [control-bracketleft] #'evim-normal-mode)
(add-hook 'evim-normal-mode-hook (lambda () (when evim-normal-mode (setq-local cursor-type t))))
(add-hook 'evim-insert-mode-hook (lambda () (when evim-insert-mode (setq-local cursor-type 'bar))))
;; (add-hook 'evim-insert-mode-hook (lambda () (when (not evim-insert-mode) (setq-local cursor-type t))))

(evim-define-mode visual)
(skey-define-keys
 '(evim-normal-mode-map evim-visual-mode-map)
 `(
   ("/" comment-line)
   ("[" ,evim-lb-keymap)
   ("]" ,evim-rb-keymap)
   ("= =" evim-indent)
   ("= a" sam-indent-all)
   ("+" goto-line)
   ("0" beginning-of-line)
   ("$" end-of-line)
   ("a" evim-a)
   ("A" evim-A)
   ("b" backward-word)
   ("c" ,evim-cut-keymap)
   ("d" ,evim-delete-keymap)
   ("e" forward-word)
   ("g" ,evim-g-keymap)
   ("G" end-of-buffer)
   ("h" backward-char)
   ("H" evim-H)
   ("i" evim-i)
   ("j" next-line)
   ("k" previous-line)
   ("l" forward-char)
   ("L" evim-L)
   ("M" move-to-window-line-top-bottom)
   ("n" isearch-repeat-forward)
   ("o" evim-open-line-below)
   ("O" evim-open-line-above)
   ("p" ,evim-paste-keymap)
   ("r" evim-replace-char)
   ("u" undo)
   ("v" evim-v)
   ("V" evim-V)
   ("y" ,evim-yank-keymap)
   ("w" forward-to-word)
   ("c" ,evim-cut-keymap)
   ("x" evim-x)
   ("u" undo)
   ("M-y" evim-yank-sexp)
   ("z" ,evim-z-keymap)
   ))

(defface evim-highlight
  '((t
     :inherit region
     ))
  "Face for evim highlighting.")

(defvar evim-region-overlay nil)
(defun evim-set-marker ()
  (interactive)
  (set-marker (mark-marker) (point)))
(defun evim-highlight-region ()
  (interactive)
  (if evim-region-overlay
      (move-overlay evim-region-overlay (marker-position (mark-marker)) (point))
    (setq evim-region-overlay (make-overlay (marker-position (mark-marker)) (point))))
  (overlay-put evim-region-overlay 'face 'evim-highlight)
  (setq-local mark-active t))
(defun evim-unhighlight-region ()
  (interactive)
  (delete-overlay evim-region-overlay)
  (setq-local mark-active nil))
(defun evim--visual-mode-enable ()
  (interactive)
  (when evim-visual-mode
    (setq-local cursor-type 'bar)
    (evim-set-marker)
    (add-hook 'post-command-hook 'evim-highlight-region)))
(add-hook 'evim-visual-mode-hook #'evim--visual-mode-enable)
(defun evim--visual-mode-disable ()
  (interactive)
  (unless evim-visual-mode
    (setq-local cursor-type t)
    (remove-hook 'post-command-hook 'evim-highlight-region)
    (evim-unhighlight-region)))
(add-hook 'evim-visual-mode-hook #'evim--visual-mode-disable)
(defun evim-kill-region ()
  (interactive)
  (kill-region (marker-position (mark-marker)) (point))
  (state-transition 'evim-visual-mode 'evim-normal-mode))
(defun evim-copy-region-as-kill ()
  (interactive)
  (copy-region-as-kill (marker-position (mark-marker)) (point))
  (state-transition 'evim-visual-mode 'evim-normal-mode))
(defun evim-visual-escape ()
  (interactive)
  (state-transition 'evim-visual-mode 'evim-normal-mode))
(defun evim-comment-region ()
  (interactive)
  (comment-or-uncomment-region (marker-position (mark-marker)) (point))
  (state-transition 'evim-visual-mode 'evim-normal-mode))
(skey-define-keys
 '(evim-visual-mode-map)
 '(
   ("/" evim-comment-region)
   ("<C-[>" evim-visual-escape)
   ("C-g" evim-visual-escape)
   ("d" evim-kill-region)
   ("y" evim-copy-region-as-kill)))

(evim-define-derived-mode term normal)
(skey-define-keys
 '(evim-normal-term-mode-map)
 `(
   ("i" (lambda () (interactive) (state-transition 'evim-normal-term-mode 'evim-insert-term-mode)))
   ))

(evim-define-derived-mode term insert)
(skey-define-keys
 '(evim-insert-term-mode-map)
 `(
   ("SPC" term-send-raw)
   ("RET" term-send-return)
   ("<tab>" (lambda () (interactive) (term-send-raw-string "\t")))
   ("C-a" term-send-home)
   ("C-b" term-send-left)
   ("C-d" term-send-del)
   ("C-e" term-send-end)
   ("C-f" term-send-right)
   ("C-h" term-send-backspace)
   ("C-n" term-send-down)
   ("C-p" term-send-up)
   ("C-v" yank)
   ("<C-[>" (lambda () (interactive) (state-transition 'evim-insert-term-mode 'evim-normal-term-mode)))
   ))

(provide 'evim)
;;; evim.el ends here
