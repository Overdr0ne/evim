;;; emotion.el --- emacs motion commands             -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Sam

;; Author: Sam <scmorris.dev@gmail.com>
;; Keywords: lisp, tools

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

(defun emotion-cmd (cmd start-motion end-motion)
  (let ((beg
         (save-excursion
           (when start-motion (funcall start-motion))
           (point)))
        (end (save-excursion
               (when end-motion (funcall end-motion))
               (point))))
    (funcall cmd beg end)))

(defmacro emotion-define-cmd (sym doc-str cmd start-motion end-motion)
  `(progn
     (defun ,sym ()
	   ,doc-str
	   (interactive)
       (emotion-cmd ,cmd ,start-motion ,end-motion))))

(provide 'emotion)
;;; emotion.el ends here
