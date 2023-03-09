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

(defun evim-visual-term-escape ()
  (interactive)
  (state-transition 'evim-visual-term-mode 'evim-normal-term-mode))

(defun evim-insert-term-escape ()
  (interactive)
  (state-transition 'evim-insert-term-mode 'evim-normal-term-mode))

(defun evim-term-A ()
  (interactive)
  (end-of-line)
  (state-transition 'evim-normal-term-mode 'evim-insert-term-mode))

(defun evim-term-a ()
  (interactive)
  (forward-char)
  (state-transition 'evim-normal-term-mode 'evim-insert-term-mode))

(defun evim-term-i ()
  (interactive)
  (state-transition 'evim-normal-term-mode 'evim-insert-term-mode))

(defun evim-term-v ()
  (interactive)
  (state-transition 'evim-normal-term-mode 'evim-visual-term-mode))

(defun evim--visual-term-mode-enable ()
  (interactive)
  (when evim-visual-term-mode
    (setq mark-active t)
    (setq-local cursor-type 'bar)
    (evim-set-marker)
    (add-hook 'post-command-hook 'evim-highlight-region)))
(add-hook 'evim-visual-term-mode-hook #'evim--visual-term-mode-enable)
(defun evim--visual-term-mode-disable ()
  (interactive)
  (unless evim-visual-term-mode
    (setq mark-active nil)
    (setq-local cursor-type t)
    (remove-hook 'post-command-hook 'evim-highlight-region)
    (evim-unhighlight-region)))
(add-hook 'evim-visual-term-mode-hook #'evim--visual-term-mode-disable)
(defun evim-term-d ()
  (interactive)
  (kill-region (marker-position (mark-marker)) (point))
  (state-transition 'evim-visual-term-mode 'evim-normal-term-mode))
(defun evim-term-y ()
  (interactive)
  (copy-region-as-kill (marker-position (mark-marker)) (point))
  (state-transition 'evim-visual-term-mode 'evim-normal-term-mode))
(defun evim-visual-term-escape ()
  (interactive)
  (state-transition 'evim-visual-term-mode 'evim-normal-term-mode))
(defun evim-term-/ ()
  (interactive)
  (comment-or-uncomment-region (marker-position (mark-marker)) (point))
  (state-transition 'evim-visual-term-mode 'evim-normal-term-mode))

(evim-define-derived-mode term normal)
(evim-define-derived-mode term visual)

(skey-define-keys
 '(evim-normal-term-mode-map)
 `(
   ("a" evim-term-a)
   ("A" evim-term-A)
   ("i" evim-term-i)
   ("v" evim-term-v)
   ))

(skey-define-keys
 '(evim-visual-term-mode-map)
 `(
   ("C-g" evim-visual-term-escape)
   ("<C-[>" evim-visual-term-escape)
   ("d" evim-term-d)
   ("y" evim-term-y)
   ))

(defun evim--insert-term-mode-disable  ()
  "Switch to line (\"cooked\") sub-mode of term mode.
This means that Emacs editing commands work as normally, until
you type \\[term-send-input] which sends the current line to the inferior."
  (interactive)
  (unless evim-insert-term-mode
    (when term-char-mode-buffer-read-only
      (setq buffer-read-only term-line-mode-buffer-read-only))
    (remove-hook 'pre-command-hook #'term-set-goto-process-mark t)
    (remove-hook 'post-command-hook #'term-goto-process-mark-maybe t)
    ))
(add-hook 'evim-insert-term-mode-hook #'evim--insert-term-mode-disable)

(defun evim--insert-term-mode-enable ()
  "Switch to char (\"raw\") sub-mode of term mode.
Each character you type is sent directly to the inferior without
intervention from Emacs, except for the escape character (usually C-c).

This command will send existing partial lines to the terminal
process."
  (interactive)
  ;; FIXME: Emit message? Cfr ilisp-raw-message
  (when evim-insert-term-mode
    (setq term-old-mode-map (current-local-map))
    (use-local-map term-raw-map)

    ;; Don't allow changes to the buffer or to point which are not
    ;; caused by the process filter.
    (when term-char-mode-buffer-read-only
      (setq buffer-read-only t))
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
    (term-update-mode-line))
  )
(add-hook 'evim-insert-term-mode-hook #'evim--insert-term-mode-enable)

(evim-define-derived-mode term insert)
(add-hook 'evim-normal-term-mode-hook (lambda () (when evim-normal-term-mode (setq-local cursor-type t))))
(add-hook 'evim-insert-term-mode-hook (lambda () (when evim-insert-term-mode (setq-local cursor-type 'bar))))
(skey-define-keys
 '(evim-insert-term-mode-map)
 `(
   ("M-;" execute-extended-command)
   ("M-SPC" execute-extended-command)
   ("<return>" (lambda () (interactive) (term-send-raw-string "\n")))
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

   ("<backspace>" term-send-backspace)
   ("SPC" term-send-raw)
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
   ("<C-[>" evim-insert-term-escape)
   ))

(defun evim-ansi-term (program &optional new-buffer-name)
  "Start a terminal-emulator in a new buffer.
This is almost the same as `term' apart from always creating a new buffer,
and `C-x' being marked as a `term-escape-char'."
  (interactive (list (read-from-minibuffer "Run program: "
					                       (or explicit-shell-file-name
					                           (getenv "ESHELL")
					                           shell-file-name))))

  ;; Pick the name of the new buffer.
  (setq term-ansi-buffer-name
	    (if new-buffer-name
	        new-buffer-name
	      (if term-ansi-buffer-base-name
	          (if (eq term-ansi-buffer-base-name t)
		          (file-name-nondirectory program)
		        term-ansi-buffer-base-name)
	        "ansi-term")))

  (setq term-ansi-buffer-name (concat "*" term-ansi-buffer-name "*"))

  ;; In order to have more than one term active at a time
  ;; I'd like to have the term names have the *term-ansi-term<?>* form,
  ;; for now they have the *term-ansi-term*<?> form but we'll see...

  (setq term-ansi-buffer-name (generate-new-buffer-name term-ansi-buffer-name))
  (setq term-ansi-buffer-name (term-ansi-make-term term-ansi-buffer-name program))

  (set-buffer term-ansi-buffer-name)
  (term-mode)
  (evim-insert-term-mode +1)

  ;; Historical baggage.  A call to term-set-escape-char used to not
  ;; undo any previous call to t-s-e-c.  Because of this, ansi-term
  ;; ended up with both C-x and C-c as escape chars.  Who knows what
  ;; the original intention was, but people could have become used to
  ;; either.   (Bug#12842)
  (let (term-escape-char)
    ;; I wanna have find-file on C-x C-f -mm
    ;; your mileage may definitely vary, maybe it's better to put this in your
    ;; .emacs ...
    (term-set-escape-char ?\C-x))

  (switch-to-buffer term-ansi-buffer-name))

(provide 'evim-term)
;;; evim-term.el ends here
