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

(defun evim--blend (a b alpha)
  "Return color A blended with color B by amount ALPHA."
  (if (cl-some #'(lambda (color) (member color '(unspecified "unspecified-fg" "unspecified-bg")))
               `(,a ,b))
      nil
    (cl-flet ((blend (a b alpha)
                     (+ (* alpha a) (* b (- 1 alpha)))))
      (-let* (((ar ag ab) (color-name-to-rgb a))
              ((br bg bb) (color-name-to-rgb b)))
        (color-rgb-to-hex (blend ar br alpha)
                          (blend ag bg alpha)
                          (blend ab bb alpha))))))

(defun evim--set-face-opacity (face percent)
  "Return FACE blended with the background by PERCENT."
  (let* ((alpha (/ percent 100.0))
		 (old-color (face-attribute face :background))
		 (new-color (evim--blend old-color (face-attribute 'default :background) alpha)))
	(message "%S" new-color)
	(set-face-attribute face nil
						:background new-color)))

(defface evim--faded-region
  (let (face)
	(copy-face 'match 'face)
	(evim--set-face-opacity 'face 30)
	face)
  "Match face faded into the background.")

(defun evim--flash-region (start end)
  "Temporarily highlight region from START to END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'region)
	(sit-for .07)
	(delete-overlay overlay)
	(setq overlay (make-overlay start end))
    (overlay-put overlay 'face 'evim--fade-flash-region)
	(sit-for 0.1)
	(delete-overlay overlay)))

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

(defmacro evim-define-interface (cmd name prefix)
  `(let ((defs
           '((,prefix ,prefix beginning-of-line (lambda () (end-of-line) (forward-char)))
             (,prefix "l" nil forward-char)
             (,prefix "h" nil backward-char)
             (,prefix "w" nil (lambda () (forward-to-word 1)))
             (,prefix "b" nil backward-word)
             (,prefix "e" nil forward-word)
             (,prefix "iw" backward-word forward-word)
             (,prefix "io" backward-sexp forward-sexp)
             ))
         (keymap (make-sparse-keymap))
         (kmap-sym ',(intern (concat "evim-" name "-keymap"))))
     (defvar kmap-sym)
     (dolist (def defs)
       (let* ((pref (nth 0 def))
              (suff (nth 1 def))
              (start-motion (nth 2 def))
              (end-motion (nth 3 def))
              (doc-str (concat "Emulate VIM " pref suff " command."))
              (cmd-sym (intern (concat "evim-" pref suff)))
              (cmd-str (concat pref suff))
              (cmd-keys (string-join (cl-subseq (split-string cmd-str "") 1 -1) " ")))
         (defalias cmd-sym
           (lambda ()
              doc-str
              (interactive)
              (evim-motion-cmd ',cmd start-motion end-motion)))
         (define-key keymap cmd-keys cmd-sym)))
     (setq kmap-sym keymap)))

(defun evim--delete (start end)
  "Delete text from start to end."
  (evim--flash-region start end)
  (kill-region start end))

(evim-define-interface evim--delete "delete" "d")

(defun evim--yank (start end)
  "Save text from START to END position."
  (evim--flash-region start end)
  (kill-ring-save start end))

(evim-define-interface evim--yank "yank" "y")

(defun evim--cut (start end)
  "Cut text from START to END position."
  (evim--delete start end)
  (evil-insert 1))

(evim-define-interface evim--cut "cut" "c")

(defun evim-paste-at (pos)
  (save-excursion
    (goto-char pos)
    (yank)))

(defun evim--paste (ignored pos)
  (save-excursion
    (goto-char pos)
    (yank)))

(evim-define-interface evim--paste "paste" "p")

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

(general-define-key
 :keymaps 'evim-paste-keymap
 "p" '(:ignore t :which-key "paste")
 "j"  #'evim-paste-pj
 "k"  #'evim-paste-pk)

(defvar evim-insert-keymap (make-composed-keymap '(text-mode-map)))
(defvar evim-normal-keymap (make-sparse-keymap))
(defvar evim-visual-keymap (make-sparse-keymap))

(defvar evim-cur-mode 'evim-normal-mode)

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
       :keymap ,(intern (concat "evim-" (symbol-name name) "-keymap"))
       :group 'evim
       (when (not (equal evim-cur-mode
                         ',(intern (concat "evim-" (symbol-name name) "-mode"))))
         (set evim-cur-mode nil)
         (setf evim-cur-mode ',(intern (concat "evim-" (symbol-name name) "-mode")))))))

(evim-define-mode insert)
(evim-define-mode normal)
(evim-define-mode visual)

(setf evim-normal-keymap
      (make-composed-keymap (list evim-delete-keymap
                                  evim-paste-keymap
                                  evim-cut-keymap)))

(provide 'evim)
;;; evim.el ends here
