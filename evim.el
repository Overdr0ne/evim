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

(require 'dash)
(require 'cl-macs)
(require 'cl-extra)

(defun evim-motion-cmd (cmd start-motion end-motion)
  (let ((beg (save-excursion
               (when start-motion (funcall start-motion))
               (point)))
        (end (save-excursion
               (when end-motion (funcall end-motion))
               (point))))
    (funcall cmd beg end)))

(defmacro evim-define-normal-cmd (sym doc-str cmd start-motion end-motion)
  `(progn
     (defun ,sym ()
	   ,doc-str
	   (interactive)
       (evim-motion-cmd ,cmd ,start-motion ,end-motion))))

(defvar root-modes
  '(term-mode-map
    Man-mode-map
    woman-mode-map
    prog-mode-map
    compilation-mode-map
    lisp-mode-map
    outline-mode-map
    help-mode-map
    helpful-mode-map
    Custom-mode-map
    text-mode-map
    shelldon-mode-map
    shell-mode-map
    conf-mode-map))
(defmacro evim-define-interface (cmd name prefix)
  `(let ((defs
           '((,prefix ,prefix beginning-of-line (lambda () (end-of-line) (forward-char)))
             (,prefix "j" nil next-line)
             (,prefix "k" nil previous-line)
             (,prefix "l" nil forward-char)
             (,prefix "h" nil backward-char)
             (,prefix "w" nil (lambda () (forward-to-word 1)))
             (,prefix "b" nil backward-word)
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
             doc-str
             (interactive)
             (evim-motion-cmd ',cmd start-motion end-motion)))
         (define-key keymap (kbd cmd-keys) cmd-sym)))
     (set kmap-sym keymap)))

(defun evim--delete (start end)
  "Delete text from start to end."
  (pulse-momentary-highlight-region start end)
  (kill-region start end))

(evim-define-interface evim--delete "delete" "d")
;; (general-define-key :keymaps root-modes :states '(visual) "d" #'kill-region)
(evim-define-normal-cmd evim-D "Emulate VIM D command." #'evim--delete nil #'end-of-line)
;; (general-define-key :keymaps root-modes :states '(normal visual) "D" #'evim-D)
(defun evim-visual-delete ()
  "Delete region."
  (interactive)
  (if rectangle-mark-mode
      (call-interactively #'kill-rectangle)
    (call-interactively #'kill-region)))
;; (general-define-key :keymaps root-modes :states '(visual) "d" #'evim-visual-delete)

(defun evim--yank (start end)
  "Save text from START to END position."
  (pulse-momentary-highlight-region start end)
  (kill-ring-save start end))

(evim-define-interface evim--yank "yank" "y")
;; (general-define-key :keymaps root-modes :states '(visual) "y" #'copy-region-as-kill)
(evim-define-normal-cmd evim-Y "Emulate VIM Y command." #'evim--yank nil #'end-of-line)
;; (general-define-key :keymaps root-modes :states '(normal visual) "Y" #'evim-Y)
;; (general-define-key :keymaps root-modes :states '(visual) "y" #'copy-region-as-kill)

(defun evim--cut (start end)
  "Cut text from START to END position."
  (evim--delete start end)
  (evil-insert 1))

(evim-define-interface evim--cut "cut" "c")
(evim-define-normal-cmd evim-C "Emulate VIM C command." #'evim--cut nil #'end-of-line)
;; (general-define-key :keymaps root-modes :states '(normal visual) "C" #'evim-C)

(defun evim-paste-at (pos)
  (save-excursion
    (goto-char pos)
    (yank)))

(defun evim--paste (_ pos)
  (save-excursion
    (goto-char pos)
    (yank)))

;; (general-define-key :keymaps root-modes :states '(normal visual) "p" nil)
(evim-define-interface evim--paste "paste" "p")
(evim-define-normal-cmd evim-P "Emulate VIM P command." #'evim--paste nil #'beginning-of-line)
;; (general-define-key :keymaps root-modes :states '(normal visual) "P" #'evim-P)

(defun evim-pp ()
  (interactive)
  (evim-paste-at (1+ (point))))

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

;; (general-define-key
;;  :states '(normal visual)
;;  :keymaps root-modes
;;  "p" '(:ignore t :which-key "paste")
;;  "pj"  #'evim-pj
;;  "pk"  #'evim-pk)

(defvar evim-insert-keymap (make-composed-keymap '(text-mode-map)))
(defvar evim-normal-keymap (make-sparse-keymap))
(defvar evim-visual-keymap (make-sparse-keymap))

(defvar evim-cur-mode 'evim-normal-mode)

(defmacro evim-define-mode (name)
  `(progn
     (defun ,(intern (concat "evim--" (symbol-name name) "-mode-enable")) nil)
     (defun ,(intern (concat "evim--" (symbol-name name) "-mode-disable")) nil)
     (define-minor-mode ,(intern (concat "evim-" (symbol-name name) "-mode"))
       ,(concat "Toggle Evim " (symbol-name name) " minor mode.

Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.")
       :init-value nil
       :lighter ,(concat " Evim:" (symbol-name name))
       :keymap ,(intern (concat "evim-" (symbol-name name) "-keymap"))
       :group 'evim
       (let ((mode-sym ',(intern (concat "evim-" (symbol-name name) "-mode")))
             (enable-sym ',(intern (concat "evim--" (symbol-name name) "-mode-enable")))
             (disable-sym ',(intern (concat "evim--" (symbol-name name) "-mode-disable"))))
         (when (not (equal evim-cur-mode mode-sym))
           (message "here")
           (funcall evim-cur-mode -1)
           (set evim-cur-mode mode-sym)
           )
         (if (symbol-value mode-sym)
             (funcall enable-sym)
           (funcall disable-sym))))))

(define-key input-decode-map
    (kbd "C-[")
    [control-leftbracket])
(defun evim-define-keys (keymaps defs)
  (dolist (def defs)
    (let* ((cmd-keys (nth 0 def))
           (cmd-sym (nth 1 def))
           (cmd-keys (if (stringp cmd-keys)
                        (kbd cmd-keys)
                      cmd-keys)))
      (dolist (keymap keymaps)
            (define-key (symbol-value keymap) cmd-keys cmd-sym)
          ))))


(evim-define-mode insert)
(evim-define-keys
 '(evim-insert-keymap)
 '(
   ("C-a" beginning-of-line)
   ("C-e" end-of-line)
   ("C-f" forward-char)
   ("C-b" backward-char)
   ("C-n" next-line)
   ("C-p" previous-line)
   ("C-s" isearch-forward)
   ("C-v" yank)
   ("M-s" isearch-repeat-forward)
   ("C-h" xah-delete-backward-char-or-bracket-text)
   ("M-h" xah-delete-backward-bracket-text)
   ("M-w" forward-word)
   ("M-b" backward-word)
   ("<return>" newline-and-indent)
   ([control-leftbracket] evim-normal-mode)
   ))

(evim-define-mode normal)
(evim-define-keys
 '(evim-normal-keymap)
 `(
   ("h" backward-char)
   ("j" next-line)
   ("k" previous-line)
   ("l" forward-char)
   ("i" evim-insert-mode)
   ("p" ,evim-paste-keymap)
   ("y" ,evim-yank-keymap)
   ("c" ,evim-cut-keymap)
   ("u" undo)
 ))

(define-key evim-normal-keymap [control-bracketleft] #'evim-normal-mode)
(defun evim--normal-mode-enable () (setq-local cursor-type t))
(defun evim--insert-mode-enable () (setq-local cursor-type 'bar))
(defun evim--insert-mode-disable () (setq-local cursor-type t))
(evim-define-mode visual)
(evim-define-keys
 '(evim-visual-keymap)
 '(
   ("d" #'kill-region)
   ("y" #'copy-region-as-kill)
   )
 )
(defun evim--visual-mode-enable () (set-mark-command nil))
(defun evim--visual-mode-disable () (keyboard-quit))

(provide 'evim)
;;; evim.el ends here
