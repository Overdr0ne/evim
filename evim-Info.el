;;; evim-Info.el --- Info modeset for evim           -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Sam

;; Author: Sam <scmorris.dev@gmail.com>
;; Keywords: Info, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the Infos of the GNU General Public License as published by
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

(evim-define-derived-mode Info normal)
(evim-define-derived-mode Info visual)
(evim-define-derived-mode Info insert)

(defun evim-Info-escape ()
  (interactive)
  (evim-transition-to 'evim-normal-Info-mode))

(defun evim-Info-A ()
  (interactive)
  (end-of-line)
  (evim-transition-to 'evim-insert-Info-mode))

(defun evim-Info-a ()
  (interactive)
  (forward-char)
  (evim-transition-to 'evim-insert-Info-mode))

(defun evim-Info-i ()
  (interactive)
  (evim-transition-to 'evim-insert-Info-mode))

(skey-define-keys
 '(evim-normal-Info-mode-map)
 `(
   ("a" evim-Info-a)
   ("A" evim-Info-A)
   ("i" evim-Info-i)
   ("v" set-mark-command)
   ))

(skey-define-keys
 '(evim-insert-Info-mode-map)
 `(
   ("<C-[>" evim-Info-escape)
   ))

(defun evim-Info--activate-mark ()
  (evim-transition-to 'evim-visual-Info-mode))
(defun evim-Info--deactivate-mark ()
  (evim-Info-escape))
(defun evim-Info--normal-mode-enable ()
  (setq-local cursor-type t)
  ;; TODO: find a cleaner way to add these hooks only when
  ;; when a given evim mode is being used.
  (evim-normal-mode -1)
  (remove-hook 'activate-mark-hook #'evim--activate-mark t)
  (remove-hook 'deactivate-mark-hook #'evim--deactivate-mark t)
  (add-hook 'activate-mark-hook #'evim-Info--activate-mark 0 t)
  (add-hook 'deactivate-mark-hook #'evim-Info--deactivate-mark 0 t)
  )
(add-hook 'evim-normal-Info-mode-on-hook #'evim-Info--normal-mode-enable)
(skey-define-keys
 '(evim-insert-Info-mode-map)
 `(
   ("<C-[>" evim-Info-escape)
   ))

(skey-define-keys
 '(evim-visual-Info-mode-map)
 `(
   ("<C-[>" keyboard-quit)
   ))

(add-hook 'Info-mode-hook #'evim-normal-Info-mode)

(provide 'evim-Info)
;;; evim-Info.el ends here
