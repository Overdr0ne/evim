;;; untermin.el --- Recode keys emitted to emulate terminals.  -*- lexical-binding: t; -*-

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

;; Recode keys emitted to emulate terminals such that they may be remapped
;; independently of their control sequence double-meanings.

;;; Code:

(define-key input-decode-map [?\C-\[] (kbd "<C-[>"))
(define-key input-decode-map [?\C-m] (kbd "<C-m>"))
(define-key input-decode-map [? ] (kbd "SPC"))
(define-key input-decode-map [?\C-j] (kbd "C-j"))

(provide 'untermin)
;;; untermin.el ends here
