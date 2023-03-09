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

(defmacro evim-define-normal-cmd (cmd prefix start-motion suffix end-motion)
  `(progn
     (defun ,(intern (concat "evim-" prefix suffix)) ()
	   ,(concat "Emulate vim " prefix suffix ".")
	   (interactive)
       (evim-motion-cmd ',cmd ',start-motion ',end-motion))))

(evim-define-normal-cmd evim--delete "penis" nil "-butthole" backward-char)

(defmacro evim-define-interface (cmd name prefix-char)
  `(progn
	 (defun ,(intern (concat "evim-" prefix-char prefix-char)) ()
	   ,(concat "Emulate vim " prefix-char prefix-char ".")
	   (interactive)
       (evim-motion-cmd ',cmd 'beginning-of-line 'end-of-line))

	 (defun ,(intern (concat "evim-" prefix-char "h")) ()
	   ,(concat "Emulate vim " prefix-char "h.")
	   (interactive)
       (evim-motion-cmd ',cmd nil 'backward-char))

	 (defun ,(intern (concat "evim-" prefix-char "j")) ()
	   ,(concat "Emulate vim " prefix-char "j.")
	   (interactive)
       (evim-motion-cmd ',cmd nil 'next-line))

	 (defun ,(intern (concat "evim-" prefix-char "k")) ()
	   ,(concat "Emulate vim " prefix-char "k.")
	   (interactive)
       (evim-motion-cmd ',cmd nil 'previous-line))

	 (defun ,(intern (concat "evim-" prefix-char "l")) ()
	   ,(concat "Emulate vim " prefix-char "l.")
	   (interactive)
	   (evim-motion-cmd ',cmd nil 'forward-char))

	 (defun ,(intern (concat "evim-" prefix-char "e")) ()
	   ,(concat "Emulate vim " prefix-char "e.")
	   (interactive)
	   (evim-motion-cmd ',cmd nil 'forward-word))

	 (defun ,(intern (concat "evim-" prefix-char "E")) ()
	   ,(concat "Emulate vim " prefix-char "E.")
	   (interactive)
       (evim-motion-cmd ',cmd nil 'end-of-line))

	 (defun ,(intern (concat "evim-" prefix-char "b")) ()
	   ,(concat "Emulate vim " prefix-char "b.")
	   (interactive)
	   (evim-motion-cmd ',cmd nil 'backward-word))

	 (defun ,(intern (concat "evim-" prefix-char "w")) ()
	   ,(concat "Emulate vim " prefix-char "w.")
	   (interactive)
	   (evim-motion-cmd ',cmd nil '(lambda () (forward-to-word 1))))

	 (defun ,(intern (concat "evim-" prefix-char "iw")) ()
	   ,(concat "Emulate vim " prefix-char "iw.")
	   (interactive)
       (evim-motion-cmd ',cmd 'backward-word 'forward-word))

	 (defun ,(intern (concat "evim-" prefix-char "io")) ()
	   ,(concat "Emulate vim " prefix-char "io.")
	   (interactive)
       (evim-motion-cmd ',cmd 'backward-sexp 'forward-sexp))

     (defvar ,(intern (concat "evim-" name "-keymap")) (make-sparse-keymap))
     (general-define-key
      :keymaps ',(intern (concat "evim-" name "-keymap"))
      ;; ,prefix-char '(:ignore t :which-key name)
      ,prefix-char  #',(intern (concat "evim-" prefix-char prefix-char))
      ,"b"  #',(intern (concat "evim-" prefix-char "b"))
      ,"e"  #',(intern (concat "evim-" prefix-char "e"))
      ,"E"  #',(intern (concat "evim-" prefix-char "E"))
      ,"iw"  #',(intern (concat "evim-" prefix-char "iw"))
      ,"io"  #',(intern (concat "evim-" prefix-char "io")))
     ))

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
 "k"  #'evim-paste-pk
 )

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

(provide 'evim)
;;; evim.el ends here
