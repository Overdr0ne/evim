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

(defvar-local evim--current-mode nil)
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
       :group 'evim
       (if ,(intern (concat "evim-" (symbol-name name) "-mode"))
           (setq evim--current-mode ',(intern (concat "evim-" (symbol-name name) "-mode")))))))

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
       :group 'evim
       (if ,(intern (concat "evim-" (symbol-name parent) "-" (symbol-name child) "-mode"))
           (progn
             (run-hooks ',(intern (concat "evim-" (symbol-name parent) "-mode-on-hook")))
             (setq evim--current-mode ',(intern (concat "evim-" (symbol-name parent) "-" (symbol-name child) "-mode"))))
         (run-hooks ',(intern (concat "evim-" (symbol-name parent) "-mode-off-hook")))))))

(defun evim-transition-to (target-mode)
  "Transition from evim--curent-mode to TARGET-MODE."
  (state-transition evim--current-mode target-mode))

(defun evim-escape ()
  (interactive)
  (evim-transition-to 'evim-normal-mode))

(evim-define-mode insert)

(defun evim--insert-mode-enable ()
  (setq-local cursor-type 'bar))
(add-hook 'evim-insert-mode-on-hook #'evim--insert-mode-enable)

(skey-define-keys
 '(evim-insert-mode-map)
 '(
   (";" self-insert-command)
   ("?" self-insert-command)
   ("SPC" self-insert-command)
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

(defun evim--activate-mark ()
  (evim-transition-to 'evim-visual-mode))
(defun evim--deactivate-mark ()
  (evim-escape))
(defun evim--normal-mode-enable ()
  (setq-local cursor-type t)
  ;; TODO: find a cleaner way to add these hooks only when
  ;; when a given evim mode is being used.
  (add-hook 'activate-mark-hook #'evim--activate-mark 0 t)
  (add-hook 'deactivate-mark-hook #'evim--deactivate-mark 0 t))
(add-hook 'evim-normal-mode-on-hook #'evim--normal-mode-enable)

(define-key evim-normal-mode-map [control-bracketleft] #'evim-normal-mode)

(skey-define-keys
 '(evim-normal-mode-map)
 `(
   ("= =" evim-indent)
   ("= a" sam-indent-all)
   ("<" evim-indent-line-left)
   (">" evim-indent-line-right)
   ("D" evim-D)
   ("P" evim-P)
   ("Y" evim-Y)
   ))

(evim-define-mode visual)
(skey-define-keys
 '(evim-normal-mode-map evim-visual-mode-map)
 `(
   ("/" comment-line)
   ("[" ,evim-lb-keymap)
   ("]" ,evim-rb-keymap)
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
   ("q" ,evim-kmacro-keymap)
   ("C-q" quit-window)
   ("r" evim-replace-char)
   ("u" undo)
   ("v" set-mark-command)
   ("V" evim-V)
   ("y" ,evim-yank-keymap)
   ("w" forward-to-word)
   ("W" forward-whitespace)
   ("c" ,evim-cut-keymap)
   ("x" evim-x)
   ("u" undo)
   ("M-y" evim-yank-sexp)
   ("z" ,evim-z-keymap)
   ))

(define-key evim-normal-mode-map (kbd "d") nil)
(define-key evim-normal-mode-map (kbd "d") evim-delete-keymap)

(defun evim--visual-mode-enable ()
  (setq-local cursor-type 'bar))
(add-hook 'evim-visual-mode-on-hook #'evim--visual-mode-enable)

(skey-define-keys
 '(evim-visual-mode-map)
 '(
   (">" indent-rigidly-right)
   ("<" indent-rigidly-left)
   ("/" comment-or-uncomment-region)
   ("<C-[>" keyboard-quit)
   ("d" kill-region)
   ("v" er/expand-region)
   ("y" copy-region-as-kill)))

(require 'evim-term)

(provide 'evim)
;;; evim.el ends here
