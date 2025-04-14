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

(evim-define-default-derived-modes 'lisp)

(skey-define-keys
 '(evim-insert-lisp-mode-map)
 `(
   ("<remap> <evim-escape>" evim-lisp-escape)
   ))

(defun evim-forward-to-sexp ()
  (interactive)
  (condition-case err
      (forward-sexp 2)
      (scan-error (forward-sexp 1)))
  (backward-sexp))

(skey-define-keys
 '(evim-normal-lisp-mode-map)
 `(
   ("M-w" evim-forward-to-sexp)
   ("M-b" backward-sexp)
   ))

(add-hook 'lisp-mode-hook #'evim-normal-lisp-mode)
(add-hook 'emacs-lisp-mode-hook #'evim-normal-lisp-mode)

(provide 'evim-lisp)
;;; evim-lisp.el ends here
