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
  (if (some #'(lambda (color) (member color '(unspecified "unspecified-fg" "unspecified-bg")))
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

(defmacro evim-define-interface (cmd prefix-char)

  `(progn
	 (defun ,(intern (concat "evim-" prefix-char prefix-char)) ()
	   ,(concat "Emulate vim " prefix-char prefix-char ".")
	   (interactive)
	   (,cmd (line-beginning-position) (line-end-position)))

	 (defun ,(intern (concat "evim-" prefix-char "e")) ()
	   ,(concat "Emulate vim " prefix-char "e.")
	   (interactive)
	   (let ((start (point))
			 (end (save-excursion
					(forward-word)
					(point))))
		 (,cmd start end)))

	 (defun ,(intern (concat "evim-" prefix-char "E")) ()
	   ,(concat "Emulate vim " prefix-char "E.")
	   (interactive)
	   (,cmd (point) (line-end-position)))

	 (defun ,(intern (concat "evim-" prefix-char "iw")) ()
	   ,(concat "Emulate vim " prefix-char "iw.")
	   (interactive)
	   (let ((start (save-excursion
					  (backward-word)
					  (point)))
			 (end (save-excursion
					(forward-word)
					(point))))
		 (,cmd start end)))

	 (defun ,(intern (concat "evim-" prefix-char "io")) ()
	   ,(concat "Emulate vim " prefix-char "io.")
	   (interactive)
	   (let ((start (save-excursion
					  (backward-sexp)
					  (point)))
			 (end (save-excursion
					(forward-sexp)
					(point))))
		 (,cmd start end)))))

(defun evim--delete (start end)
  "Delete text from start to end."
  (evim--flash-region start end)
  (kill-region start end))

(evim-define-interface evim--delete "d")

(defun evim--yank (start end)
  "Save text from START to END position."
  (evim--flash-region start end)
  (kill-ring-save start end))

(evim-define-interface evim--yank "y")

(defun evim--cut (start end)
  "Cut text from START to END position."
  (evim--delete start end)
  (evil-insert 1))

(evim-define-interface evim--cut "c")

(provide 'evim)
;;; evim.el ends here
