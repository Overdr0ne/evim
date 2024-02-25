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

(evim-define-derived-mode python normal)
(evim-define-derived-mode python visual)
(evim-define-derived-mode python insert)

(defun evim-python-escape ()
  (interactive)
  (evim-transition-to 'evim-normal-python-mode))

(defun evim-python-A ()
  (interactive)
  (end-of-line)
  (evim-transition-to 'evim-insert-python-mode))

(defun evim-python-a ()
  (interactive)
  (forward-char)
  (evim-transition-to 'evim-insert-python-mode))

(defun evim-python-i ()
  (interactive)
  (evim-transition-to 'evim-insert-python-mode))

;; (defun evim-python-v ()
;;   (interactive)
;;   (evim-transition-to 'evim-visual-python-mode))

(skey-define-keys
 '(evim-normal-python-mode-map)
 `(
   ("a" evim-python-a)
   ("A" evim-python-A)
   ("i" evim-python-i)
   ("v" set-mark-command)
   ))

(skey-define-keys
 '(evim-insert-python-mode-map)
 `(
   ("<C-[>" evim-python-escape)
   ))

(defun evim-python--activate-mark ()
  (evim-transition-to 'evim-visual-python-mode))
(defun evim-python--deactivate-mark ()
  (evim-python-escape))
(defun evim-python--normal-mode-enable ()
  (setq-local cursor-type t)
  ;; TODO: find a cleaner way to add these hooks only when
  ;; when a given evim mode is being used.
  (evim-normal-mode -1)
  (remove-hook 'activate-mark-hook #'evim--activate-mark t)
  (remove-hook 'deactivate-mark-hook #'evim--deactivate-mark t)
  (add-hook 'activate-mark-hook #'evim-python--activate-mark 0 t)
  (add-hook 'deactivate-mark-hook #'evim-python--deactivate-mark 0 t)
  )
(add-hook 'evim-normal-python-mode-on-hook #'evim-python--normal-mode-enable)
(skey-define-keys
 '(evim-insert-python-mode-map)
 `(
   ("<C-[>" evim-python-escape)
   ))

(skey-define-keys
 '(evim-visual-python-mode-map)
 `(
   (">" python-indent-shift-right)
   ("<" python-indent-shift-left)
   ("<C-[>" keyboard-quit)
   ))

(add-hook 'python-mode-hook #'evim-normal-python-mode)

(provide 'evim-python)
;;; evim-python.el ends here
