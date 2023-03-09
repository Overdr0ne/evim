;;; state.el --- A library for creating states from minor modes.  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Sam

;; Author: Sam <scmorris.dev@gmail.com>
;; Keywords: languages, tools

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

;; This library provides a handful of functions that operate on minor-modes as
;; if they were states.

;;; Code:

(defun state-transition (mode-src mode-tar)
  (if (not mode-src)
      (error "state: cannot transition, mode-src %S not enabled" (symbol-name mode-src))
    (funcall mode-src -1)
    (message "evim: %s disabled..." mode-src)
    (funcall mode-tar +1))
    (message "evim: %s enabled..." mode-tar)
  )

(defun state-make-transition-cmd (mode-src mode-tar)
  (lambda () (interactive) (state-transition mode-src mode-tar)))

(provide 'state)
;;; state.el ends here
