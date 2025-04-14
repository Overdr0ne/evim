;;; evim-term.el --- terminal modeset for evim       -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Overdr0ne

;; Author: Overdr0ne <scmorris.dev@gmail.com>
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

(require 'term)

(evim-define-derived-mode term normal)
(evim-define-derived-mode term visual)
(evim-define-derived-mode term insert)

(defun evim-term-escape ()
  (interactive)
  (evim-transition-to 'evim-normal-term-mode))

(defun evim-term-A ()
  (interactive)
  (end-of-line)
  (evim-transition-to 'evim-insert-term-mode))

(defun evim-term-a ()
  (interactive)
  (forward-char)
  (evim-transition-to 'evim-insert-term-mode))

(defun evim-term-i ()
  (interactive)
  (evim-transition-to 'evim-insert-term-mode))

(defun evim--visual-term-mode-enable ()
  )
(add-hook 'evim-visual-term-mode-on-hook #'evim--visual-term-mode-enable)

(defun evim--normal-term-mode-enable ()
  )
(add-hook 'evim-normal-term-mode-on-hook #'evim--normal-term-mode-enable)

(defun evim-primary-yank ()
          (interactive)
          (term-primary-yank)
          (evim-transition-to 'evim-insert-term-mode))

(skey-define-keys
 '(evim-normal-term-mode-map)
 `(
   ("a" evim-term-a)
   ("A" evim-term-A)
   ("i" evim-term-i)
   ("p" evim-primary-yank)
))

(defun evim--insert-term-mode-disable  ()
  "Switch to line (\"cooked\") sub-mode of term mode.
This means that Emacs editing commands work as normally, until
you type \\[term-send-input] which sends the current line to the inferior."
  (interactive)

  (remove-hook 'pre-command-hook #'term-set-goto-process-mark t)
  (remove-hook 'post-command-hook #'term-goto-process-mark-maybe t))
(add-hook 'evim-insert-term-mode-off-hook #'evim--insert-term-mode-disable)

(defun evim--insert-term-mode-enable ()
  "Switch to char (\"raw\") sub-mode of term mode.
Each character you type is sent directly to the inferior without
intervention from Emacs, except for the escape character (usually C-c).

This command will send existing partial lines to the terminal
process."
  (interactive)

  (setq term-old-mode-map (current-local-map))
  (use-local-map term-raw-map)

  ;; Don't allow changes to the buffer or to point which are not
  ;; caused by the process filter.
  (setq buffer-read-only t)
  (add-hook 'pre-command-hook #'term-set-goto-process-mark nil t)
  (add-hook 'post-command-hook #'term-goto-process-mark-maybe nil t)

  ;; Send existing partial line to inferior (without newline).
  (let ((pmark (process-mark (get-buffer-process (current-buffer)))))
    (when (> (point) pmark)
	  (unwind-protect
	      (progn
	        (add-function :override term-input-sender #'term-send-string)
	        (end-of-line)
	        (term-send-input))
	    (remove-function term-input-sender #'term-send-string))))
  )
(add-hook 'evim-insert-term-mode-on-hook #'evim--insert-term-mode-enable)

(skey-define-keys
 '(evim-insert-term-mode-map)
 `(
   ("M-;" execute-extended-command)
   ("M-SPC" execute-extended-command)
   ("<return>" (lambda () (interactive) (term-send-raw-string "\C-m")))
   ("<tab>" (lambda () (interactive) (term-send-raw-string "\t")))
   ("C-a" term-send-raw)
   ("C-b" term-send-left)
   ("C-d" term-delchar-or-maybe-eof)
   ("C-e" term-send-end)
   ("C-f" term-send-right)
   ("C-h" term-send-backspace)
   ("C-m" (lambda () (interactive) (term-send-raw-string "\C-m")))
   ("C-n" term-send-down)
   ("C-p" term-send-up)
   ("C-v" term-primary-yank)

   ("<backspace>" term-send-backspace)
   ("SPC" term-send-raw)
   ("~" term-send-raw)
   ("!" term-send-raw)
   ("@" term-send-raw)
   ("#" term-send-raw)
   ("$" term-send-raw)
   ("%" term-send-raw)
   ("^" term-send-raw)
   ("&" term-send-raw)
   ("*" term-send-raw)
   ("(" term-send-raw)
   (")" term-send-raw)
   ("|" term-send-raw)
   ("\\" term-send-raw)
   ("/" term-send-raw)
   ("'" term-send-raw)
   ("`" term-send-raw)
   ("\"" term-send-raw)
   ("-" term-send-raw)
   ("=" term-send-raw)

   ("1" term-send-raw)
   ("2" term-send-raw)
   ("3" term-send-raw)
   ("4" term-send-raw)
   ("5" term-send-raw)
   ("6" term-send-raw)
   ("7" term-send-raw)
   ("8" term-send-raw)
   ("9" term-send-raw)
   ("0" term-send-raw)
   ("_" term-send-raw)
   ("+" term-send-raw)
   ("[" term-send-raw)
   ("{" term-send-raw)
   ("]" term-send-raw)
   ("}" term-send-raw)
   ("<" term-send-raw)
   ("," term-send-raw)
   (">" term-send-raw)
   ("." term-send-raw)
   ("?" term-send-raw)
   (":" term-send-raw)
   (";" term-send-raw)

   ("a" term-send-raw)
   ("b" term-send-raw)
   ("c" term-send-raw)
   ("d" term-send-raw)
   ("e" term-send-raw)
   ("f" term-send-raw)
   ("g" term-send-raw)
   ("h" term-send-raw)
   ("i" term-send-raw)
   ("j" term-send-raw)
   ("k" term-send-raw)
   ("l" term-send-raw)
   ("m" term-send-raw)
   ("n" term-send-raw)
   ("o" term-send-raw)
   ("p" term-send-raw)
   ("q" term-send-raw)
   ("r" term-send-raw)
   ("s" term-send-raw)
   ("t" term-send-raw)
   ("u" term-send-raw)
   ("v" term-send-raw)
   ("w" term-send-raw)
   ("x" term-send-raw)
   ("y" term-send-raw)
   ("z" term-send-raw)

   ("A" term-send-raw)
   ("B" term-send-raw)
   ("C" term-send-raw)
   ("D" term-send-raw)
   ("E" term-send-raw)
   ("F" term-send-raw)
   ("G" term-send-raw)
   ("H" term-send-raw)
   ("I" term-send-raw)
   ("J" term-send-raw)
   ("K" term-send-raw)
   ("L" term-send-raw)
   ("M" term-send-raw)
   ("N" term-send-raw)
   ("O" term-send-raw)
   ("P" term-send-raw)
   ("Q" term-send-raw)
   ("R" term-send-raw)
   ("S" term-send-raw)
   ("T" term-send-raw)
   ("U" term-send-raw)
   ("V" term-send-raw)
   ("W" term-send-raw)
   ("X" term-send-raw)
   ("Y" term-send-raw)
   ("Z" term-send-raw)
   ("<remap> <evim-escape>" evim-term-escape)
   ))

(defun evim--term-activate-mark ()
  (evim-transition-to 'evim-visual-term-mode))
(defun evim--term-deactivate-mark ()
  (evim-term-escape))
(defun evim-term-setup (program)
  (setq term-ansi-buffer-name
	    (if term-ansi-buffer-base-name
	        (if (eq term-ansi-buffer-base-name t)
		        (file-name-nondirectory program)
		      term-ansi-buffer-base-name)
	      "ansi-term"))

  (setq term-ansi-buffer-name (concat "*" term-ansi-buffer-name "*"))
  (setq term-ansi-buffer-name (generate-new-buffer-name term-ansi-buffer-name))
  (setq term-ansi-buffer-name (term-ansi-make-term term-ansi-buffer-name program))
  (set-buffer term-ansi-buffer-name)

  (term-mode)
  (evim-insert-term-mode +1)

  (add-hook 'activate-mark-hook #'evim--term-activate-mark 0 t)
  (add-hook 'deactivate-mark-hook #'evim--term-deactivate-mark 0 t)
  )

(defun evim--term (program)
  "Start a terminal-emulator in a new buffer.
This is almost the same as `term' apart from always creating a new buffer,
and `C-x' being marked as a `term-escape-char'."
  (evim-term-setup program)
  (switch-to-buffer term-ansi-buffer-name))
(defun evim-term ()
  (interactive)

  (evim--term (getenv "SHELL")))

(defun evim-term-side ()
  "Start a terminal-emulator in a new buffer.
This is almost the same as `term' apart from always creating a new buffer,
and `C-x' being marked as a `term-escape-char'."
  (interactive)

  (evim-term-setup (getenv "SHELL"))
  (pop-to-buffer term-ansi-buffer-name))

(dolist (hook '(shell-mode-hook))
  (dolist (fun '(evim-insert-term-mode))
    (add-hook hook fun)))

(provide 'evim-term)
;;; evim-term.el ends here
