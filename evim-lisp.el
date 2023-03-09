;;; evim-lisp.el --- lisp modeset for evim           -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Sam

;; Author: Sam <scmorris.dev@gmail.com>
;; Keywords: lisp, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the lisps of the GNU General Public License as published by
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

(evim-define-derived-mode lisp normal)
(evim-define-derived-mode lisp visual)
(evim-define-derived-mode lisp insert)

(defun evim-lisp-escape ()
  (interactive)
  (evim-transition-to 'evim-normal-lisp-mode))

(defun evim-lisp-A ()
  (interactive)
  (end-of-line)
  (evim-transition-to 'evim-insert-lisp-mode))

(defun evim-lisp-a ()
  (interactive)
  (forward-char)
  (evim-transition-to 'evim-insert-lisp-mode))

(defun evim-lisp-i ()
  (interactive)
  (evim-transition-to 'evim-insert-lisp-mode))

(skey-define-keys
 '(evim-normal-lisp-mode-map)
 `(
   ("i" sam-insert-at-end-of-form)))

(skey-define-keys
 '(evim-normal-lisp-mode-map)
 `(
   ("a" evim-lisp-a)
   ("A" evim-lisp-A)
   ("i" evim-lisp-i)))

(skey-define-keys
 '(evim-insert-lisp-mode-map)
 `(
   ("<C-[>" evim-lisp-escape)
))

(provide 'evim-lisp)
;;; evim-lisp.el ends here
