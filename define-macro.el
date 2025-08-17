;;; define-kmacro.el --- mark variant for operating on lines  -*- lexical-binding: t; -*-

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

;;; Code:

(defvar kmacro-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap keyboard-quit] 'define-kmacro-mode-disable)
    (define-key map "@" #'define-kmacro-mode-disable)
    map)
  "Transient keymap activated during define-kmacro-mode.")

(define-minor-mode define-kmacro-mode
  "A transient mode to define keyboard macros."
  :global t
  :keymap kmacro-mode-map
  (if define-kmacro-mode
      (progn
        (push `(define-kmacro-mode . ,kmacro-mode-map)
              minor-mode-map-alist))
    (setf minor-mode-map-alist (assoc-delete-all 'define-kmacro-mode minor-mode-map-alist))))

(defun define-kmacro-mode-disable ()
  "Disable kmacro mode."
  (interactive)
  (define-kmacro-mode -1)
  (kmacro-end-macro nil))

(defun define-kmacro-mode-enable ()
  "Enable kmacro mode."
  (interactive)
  (define-kmacro-mode +1)
  (kmacro-start-macro nil))

(provide 'define-kmacro)
;;; define-kmacro.el ends here
