;;; evim-lib.el --- library of emulated vim functions and aliases  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Sam

;; Author: Sam <scmorris.dev@gmail.com>
;; Keywords: lisp

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

(require 'untermin)
(require 'dash)
(require 'cl-macs)
(require 'cl-extra)
(require 'state)
(require 'skey)
(require 'emotion)
(require 'pulse)

(defmacro evim-define-interface (cmd name prefix)
  `(let ((defs
           '((,prefix ,prefix beginning-of-line (lambda () (end-of-line) (forward-char)))
             (,prefix "j" nil next-line)
             (,prefix "k" nil previous-line)
             (,prefix "l" nil forward-char)
             (,prefix "h" nil backward-char)
             (,prefix "w" nil (lambda () (forward-to-word 1)))
             (,prefix "W" nil (lambda () (forward-whitespace 1)))
             (,prefix "b" nil (lambda () (forward-word -1)))
             (,prefix "B" nil (lambda () (forward-whitespace -1)))
             (,prefix "e" nil forward-word)
             (,prefix "iw" backward-word forward-word)
             (,prefix "io" backward-sexp forward-sexp)))
         (keymap (make-sparse-keymap))
         (kmap-sym ',(intern (concat "evim-" name "-keymap"))))
     (defvar ,(intern (concat "evim-" name "-keymap")))
     (dolist (def defs)
       (let* ((pref (nth 0 def))
              (suff (nth 1 def))
              (start-motion (nth 2 def))
              (end-motion (nth 3 def))
              (doc-str (concat "Emulate VIM " pref suff " command."))
              (cmd-sym (intern (concat "evim-" pref suff)))
              (cmd-str (concat pref suff))
              (cmd-keys (string-join (cl-subseq (split-string suff "") 1 -1) " ")))
         (defalias cmd-sym
           (lambda ()
             (interactive)
             (emotion-cmd ',cmd start-motion end-motion))
           doc-str)
         (define-key keymap (kbd cmd-keys) cmd-sym)
         ))
     (set kmap-sym keymap)))

(defun evim--delete (beg end)
  "Delete text from start to end."
  (pulse-momentary-highlight-region beg end)
  (sit-for (+ pulse-delay .1))
  (kill-region beg end))

(evim-define-interface evim--delete "delete" "d")
(skey-define-keys
 '(evim-delete-keymap)
 `(
   ("M-w" delete-forward-sexp)
   ("M-b" delete-backward-sexp)
   ))
(emotion-define-cmd evim-D "Emulate VIM D command." #'evim--delete nil #'end-of-line)
(defun evim-visual-delete ()
  "Delete region."
  (interactive)
  (if rectangle-mark-mode
      (call-interactively #'kill-rectangle)
    (call-interactively #'kill-region)))

(defun evim--yank (start end)
  "Save text from START to END position."
  (pulse-momentary-highlight-region start end)
  (copy-region-as-kill start end))

(evim-define-interface evim--yank "yank" "y")
(skey-define-keys
 '(evim-yank-keymap)
 `(
   ("M-w" yank-forward-sexp)
   ("M-b" yank-backward-sexp)
   ))
(emotion-define-cmd evim-Y "Emulate VIM Y command." #'evim--yank nil #'end-of-line)
(emotion-define-cmd evim-yank-sexp "Kill the following sexp." #'evim--yank nil #'forward-sexp)

(defun evim--cut (start end)
  "Cut text from START to END position."
  (evim--delete start end)
  (evil-insert 1))

(evim-define-interface evim--cut "cut" "c")
(emotion-define-cmd evim-C "Emulate VIM C command." #'evim--cut nil #'end-of-line)

(defun evim-paste-at (pos)
  (save-excursion
    (goto-char pos)
    (insert (current-kill 0))))

(defun evim--paste (_ pos)
  (save-excursion
    (goto-char pos)
    (insert (current-kill 0))))

(evim-define-interface evim--paste "paste" "p")
(emotion-define-cmd evim-P "Emulate VIM P command." #'evim--paste nil #'beginning-of-line)

(defun evim-pp ()
  (interactive)
  (evim-paste-at (point)))

(defun evim-pP ()
  (interactive)
  (evim-paste-at (point)))

(defun evim-pk ()
  "Insert an empty line above the current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (evim-paste-at (point))))

(defun evim-pj ()
  "Insert an empty line above the current line."
  (interactive)
  (evim-paste-at (1+ (line-end-position))))

(defun evim-open-line-below ()
  (interactive)
  (end-of-line)
  (newline-and-indent)
  (evim-transition-to 'evim-insert-mode))

(defun evim-open-line-above ()
  (interactive)
  (beginning-of-line)
  (save-excursion (insert "\n"))
  (indent-according-to-mode)
  (evim-transition-to 'evim-insert-mode))

(defun evim-join ()
  (interactive)
  (save-excursion
    (end-of-line)
    (forward-char)
    (join-line)))

(defun evim-A ()
  (interactive)
  (end-of-line)
  (evim-transition-to 'evim-insert-mode))

(defun evim-a ()
  (interactive)
  (forward-char)
  (evim-transition-to 'evim-insert-mode))

(defun evim-H ()
  (interactive)
  (move-to-window-line 0))

(defun evim-i ()
  (interactive)
  (evim-transition-to 'evim-insert-mode))

(defun evim-L ()
  (interactive)
  (move-to-window-line -1))

(defun evim-V ()
  (interactive)
  (save-excursion
    (next-line)
    (beginning-of-line)
    (evim-transition-to 'evim-visual-mode)))

(defun evim-x ()
  (interactive)
  (delete-char 1))

(defun evim-replace-char (newc)
  (interactive "sEnter char: ")
  (save-excursion
    (let* ((beg (point))
           (end (1+ (point)))
           (oldc (buffer-substring-no-properties beg end)))
      (replace-string-in-region oldc newc beg end))))

(defun evim-indent ()
  (interactive)
  (indent-according-to-mode))

(defvar evim-lb-keymap (make-sparse-keymap))
(defun backward-end-of-defun ()
  (interactive)
  (end-of-defun -1))
(skey-define-keys
 '(evim-lb-keymap)
 `(
   ("(" backward-up-list)
   ("[" beginning-of-defun)
   ("]" backward-end-of-defun)
   ))
(defvar evim-rb-keymap (make-sparse-keymap))
(defun forward-beginning-of-defun ()
  (interactive)
  (beginning-of-defun -1))
(skey-define-keys
 '(evim-rb-keymap)
 `(
   (")" up-list)
   ("[" forward-beginning-of-defun)
   ("]" end-of-defun)
   ))

(defvar evim-g-keymap (make-sparse-keymap))
(skey-define-keys
 '(evim-g-keymap)
 `(
   ("f" find-file-at-point)
   ("g" beginning-of-buffer)
   ("G" end-of-buffer)))

(defvar evim-z-keymap (make-sparse-keymap))
(skey-define-keys
 '(evim-z-keymap)
 `(
   ("z" recenter)))

(defvar evim-hjkl-keymap (make-sparse-keymap))
(skey-define-keys
 '(evim-hjkl-keymap)
 '(
   ("h" backward-char)
   ("j" next-line)
   ("k" previous-line)
   ("l" forward-char)
   ))

(defun evim-kmacro-insert-last ()
  (interactive)
  (kmacro-start-macro-or-insert-counter ))

(defvar evim-kmacro-keymap (make-sparse-keymap))
(skey-define-keys
 '(evim-kmacro-keymap)
 '(
   ("s" kmacro-start-macro-or-insert-counter)
   ("e" kmacro-end-or-call-macro)
   ("i" kmacro-insert-counter)
   ("q" consult-kmacro)
   ))

(defun evim-indent-line-left ()
  (interactive)
  (let* ((beg (line-beginning-position))
         (end (line-end-position)))
    (indent-rigidly-left beg end)))

(defun evim-indent-line-right ()
  (interactive)
  (let* ((beg (line-beginning-position))
         (end (line-end-position)))
    (indent-rigidly-right beg end)))

(provide 'evim-lib)
;;; evim-lib.el ends here
