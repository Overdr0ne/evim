;;; evim-python.el --- python modeset for evim           -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Sam

;; Author: Sam <scmorris.dev@gmail.com>
;; Keywords: python, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the pythons of the GNU General Public License as published by
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

(require 'evim-lib)

(evim-define-default-derived-modes 'python)

(skey-define-keys
 '(evim-insert-python-mode-map)
 `(
   ("<remap> <evim-escape>" evim-python-escape)
   ))

(skey-define-keys
 '(evim-visual-python-mode-map)
 `(
   (">" python-indent-shift-right)
   ("<" python-indent-shift-left)
   ))

(add-hook 'python-mode-hook #'evim-normal-python-mode)

(provide 'evim-python)
;;; evim-python.el ends here
