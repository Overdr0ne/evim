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

(require 'dash)
(require 'cl-macs)
(require 'cl-extra)
(require 'state)
(require 'skey)
(require 'evim-lib)
(require 'line-mark)

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

(defun evim-define-default-derived-modes (child)
  (eval
   `(progn
      (evim-define-derived-mode ,child normal)
      (evim-define-derived-mode ,child visual)
      (evim-define-derived-mode ,child insert)
      ;; Setup all the weird hooks necessary to automatically switch to/from visual mode
      ;; when the mark is activated/deactivated
      (defun ,(intern (concat "evim-" (symbol-name child) "-escape")) ()
        (interactive)
        (evim-transition-to ',(intern (concat "evim-normal-" (symbol-name child) "-mode"))))
      (defun ,(intern (concat "evim-" (symbol-name child) "--activate-mark")) ()
        (evim-transition-to ',(intern (concat "evim-visual-" (symbol-name child) "-mode"))))
      (defun ,(intern (concat "evim-" (symbol-name child) "--deactivate-mark")) ()
        (,(intern (concat "evim-" (symbol-name child) "-escape"))))
      (defun ,(intern (concat "evim-" (symbol-name child) "--normal-mode-enable")) ()
        (setq-local cursor-type t)
        ;; TODO: find a cleaner way to add these hooks only when
        ;; when a given evim mode is being used.
        (evim-normal-mode -1)
        (remove-hook 'activate-mark-hook #'evim--activate-mark t)
        (remove-hook 'deactivate-mark-hook #'evim--deactivate-mark t)
        (add-hook 'activate-mark-hook
                  #',(intern (concat "evim-" (symbol-name child) "--activate-mark")) 0 t)
        (add-hook 'deactivate-mark-hook
                  #',(intern (concat "evim-" (symbol-name child) "--deactivate-mark")) 0 t))
      (add-hook ',(intern (concat "evim-normal-" (symbol-name child) "-mode-on-hook"))
                #',(intern (concat "evim-" (symbol-name child) "--normal-mode-enable")))

      (defun ,(intern (concat "evim-" (symbol-name child) "-A")) ()
        (interactive)
        (end-of-line)
        (evim-transition-to ',(intern (concat "evim-insert-" (symbol-name child) "-mode"))))

      (defun ,(intern (concat "evim-" (symbol-name child) "-a")) ()
        (interactive)
        (forward-char)
        (evim-transition-to ',(intern (concat "evim-insert-" (symbol-name child) "-mode"))))

      (defun ,(intern (concat "evim-" (symbol-name child) "-i")) ()
        (interactive)
        (evim-transition-to ',(intern (concat "evim-insert-" (symbol-name child) "-mode"))))

      (skey-define-keys
       (list ',(intern (concat "evim-normal-" (symbol-name child) "-mode-map")))
       (list
        (list "a" ',(intern (concat "evim-" (symbol-name child) "-a")))
        (list "A" ',(intern (concat "evim-" (symbol-name child) "-A")))
        (list "i" ',(intern (concat "evim-" (symbol-name child) "-i")))))

      (skey-define-keys
       (list ',(intern (concat "evim-insert-" (symbol-name child) "-mode-map")))
       (list
        (list "<C-[>" ',(intern (concat "evim-" (symbol-name child) "-escape"))))))))

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
 `(
   ("M-x" execute-extended-command)
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
   ("<C-[>" evim-escape)
   )
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

(skey-define-keys
 '(evim-normal-mode-map)
 `(
   ("= =" evim-indent)
   ("= a" sam-indent-all)
   ("<" evim-indent-line-left)
   (">" evim-indent-line-right)
   ))

(evim-define-mode visual)
(skey-define-keys
 '(evim-normal-mode-map evim-visual-mode-map)
 `(
   ("1" digit-argument)
   ("2" digit-argument)
   ("3" digit-argument)
   ("4" digit-argument)
   ("5" digit-argument)
   ("6" digit-argument)
   ("7" digit-argument)
   ("8" digit-argument)
   ("9" digit-argument)
   ("0" beginning-of-line)
   ("!" nil)
   ("@" nil)
   ("#" nil)
   ("$" end-of-line)
   ("%" nil)
   ("^" nil)
   ("&" nil)
   ("*" nil)
   ("(" nil)
   (")" nil)
   ("-" previous-line)
   ("+" next-line)
   ("/" comment-line)
   ("[" ,evim-lb-keymap)
   ("]" ,evim-rb-keymap)
   ("a" evim-a)
   ("A" evim-A)
   ("b" backward-word)
   ("B" evim-B)
   ("c" ,evim-cut-keymap)
   ("C" nil)
   ("d" ,evim-delete-keymap)
   ("D" evim-D)
   ("e" forward-word)
   ("E" end-of-line)
   ("f" nil)
   ("F" nil)
   ("g" ,evim-g-keymap)
   ("G" end-of-buffer)
   ("h" backward-char)
   ("H" evim-H)
   ("i" evim-i)
   ("I" evim-I)
   ("j" next-line)
   ("J" evim-join)
   ("k" previous-line)
   ("K" help)
   ("l" forward-char)
   ("L" evim-L)
   ("m" point-to-register)
   ("M" move-to-window-line-top-bottom)
   ("n" isearch-repeat-forward)
   ("N" isearch-repeat-backward)
   ("o" evim-open-line-below)
   ("O" evim-open-line-above)
   ("p" ,evim-paste-keymap)
   ("P" evim-P)
   ("q" ,evim-kmacro-keymap)
   ("C-q" quit-window)
   ("r" evim-replace-char)
   ("R" overwrite-mode)
   ("s" nil)
   ("S" nil)
   ("u" undo)
   ("U" undo-redo)
   ("v" set-mark-command)
   ("C-v" rectangle-mark-mode)
   ("V" line-mark-mode-activate)
   ("w" forward-to-word)
   ("W" forward-whitespace)
   ("x" evim-x)
   ("X" nil)
   ("y" ,evim-yank-keymap)
   ("Y" evim-Y)
   ("M-y" evim-yank-sexp)
   ("z" ,evim-z-keymap)))

(define-key evim-normal-mode-map (kbd "d") nil)
(define-key evim-normal-mode-map (kbd "d") evim-delete-keymap)

(defun evim--visual-mode-enable ()
  (setq-local cursor-type 'bar))
(add-hook 'evim-visual-mode-on-hook #'evim--visual-mode-enable)

(skey-define-keys
 '(evim-visual-mode-map)
 '(
   ("M-x" execute-extended-command)
   ("=" indent-region)
   (">" indent-rigidly-right)
   ("<" indent-rigidly-left)
   ("/" comment-or-uncomment-region)
   ("<C-[>" keyboard-quit)
   ("c" evim-visual-cut)
   ("d" kill-region)
   ("p" evim-replace-region-with-kill)
   ("s" isearch-forward)
   ("v" er/expand-region)
   ("y" copy-region-as-kill)))

(require 'evim-term)
(require 'evim-lisp)
(require 'evim-python)
(require 'evim-clojure)
(require 'evim-Info)
(require 'evim-helpful)

(provide 'evim)
;;; evim.el ends here
