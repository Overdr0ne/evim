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

(add-to-list 'load-path (file-name-directory (f-this-file)))

(require 'untermin)
(require 'dash)
(require 'cl-macs)
(require 'cl-extra)
(require 'state)

(defun evim-motion-cmd (cmd start-motion end-motion)
  (let ((beg (save-excursion
               (when start-motion (funcall start-motion))
               (point)))
        (end (save-excursion
               (when end-motion (funcall end-motion))
               (point))))
    (funcall cmd beg end)))

(defmacro evim-define-normal-cmd (sym doc-str cmd start-motion end-motion)
  `(progn
     (defun ,sym ()
	   ,doc-str
	   (interactive)
       (evim-motion-cmd ,cmd ,start-motion ,end-motion))))

(defvar root-modes
  '(term-mode-map
    Man-mode-map
    woman-mode-map
    prog-mode-map
    compilation-mode-map
    lisp-mode-map
    outline-mode-map
    help-mode-map
    helpful-mode-map
    Custom-mode-map
    text-mode-map
    shelldon-mode-map
    shell-mode-map
    conf-mode-map))
(defmacro evim-define-interface (cmd name prefix)
  `(let ((defs
           '((,prefix ,prefix beginning-of-line (lambda () (end-of-line) (forward-char)))
             (,prefix "j" nil next-line)
             (,prefix "k" nil previous-line)
             (,prefix "l" nil forward-char)
             (,prefix "h" nil backward-char)
             (,prefix "w" nil (lambda () (forward-to-word 1)))
             (,prefix "b" nil backward-word)
             (,prefix "e" nil forward-word)
             (,prefix "iw" backward-word forward-word)
             (,prefix "io" backward-sexp forward-sexp)))
         (keymap (make-sparse-keymap))
         (kmap-sym ',(intern (concat "evim-" name "-keymap"))))
     (defvar ,(intern (concat "evim-" name "-keymap")))
     (dolist (def defs)
       (let* ((pref (nth 0 def))
              (suff (nth 1 def))
              (start-motion (nth 2 def))
              (end-motion (nth 3 def))
              (doc-str (concat "Emulate VIM " pref suff " command."))
              (cmd-sym (intern (concat "evim-" pref suff)))
              (cmd-str (concat pref suff))
              (cmd-keys (string-join (cl-subseq (split-string suff "") 1 -1) " ")))
         (defalias cmd-sym
           (lambda ()
             doc-str
             (interactive)
             (evim-motion-cmd ',cmd start-motion end-motion)))
         (define-key keymap (kbd cmd-keys) cmd-sym)))
     (set kmap-sym keymap)))

(defun evim--delete (start end)
  "Delete text from start to end."
  (pulse-momentary-highlight-region start end)
  (kill-region start end))

(evim-define-interface evim--delete "delete" "d")
(evim-define-normal-cmd evim-D "Emulate VIM D command." #'evim--delete nil #'end-of-line)
(defun evim-visual-delete ()
  "Delete region."
  (interactive)
  (if rectangle-mark-mode
      (call-interactively #'kill-rectangle)
    (call-interactively #'kill-region)))
;; (general-define-key :keymaps root-modes :states '(visual) "d" #'evim-visual-delete)

(defun evim--yank (start end)
  "Save text from START to END position."
  (pulse-momentary-highlight-region start end)
  (kill-ring-save start end))

(evim-define-interface evim--yank "yank" "y")
(evim-define-normal-cmd evim-Y "Emulate VIM Y command." #'evim--yank nil #'end-of-line)
(evim-define-normal-cmd evim-yank-sexp "Kill the following sexp." #'evim--yank nil #'forward-sexp)

(defun evim--cut (start end)
  "Cut text from START to END position."
  (evim--delete start end)
  (evil-insert 1))

(evim-define-interface evim--cut "cut" "c")
(evim-define-normal-cmd evim-C "Emulate VIM C command." #'evim--cut nil #'end-of-line)

(defun evim-paste-at (pos)
  (save-excursion
    (goto-char pos)
    (yank)))

(defun evim--paste (_ pos)
  (save-excursion
    (goto-char pos)
    (yank)))

;; (general-define-key :keymaps root-modes :states '(normal visual) "p" nil)
(evim-define-interface evim--paste "paste" "p")
(evim-define-normal-cmd evim-P "Emulate VIM P command." #'evim--paste nil #'beginning-of-line)

(defun evim-pp ()
  (interactive)
  (evim-paste-at (1+ (point))))

(defun evim-pP ()
  (interactive)
  (evim-paste-at (point)))

(defun evim-pk ()
  "Insert an empty line above the current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (evim-paste-at (point))))

(defun evim-pj ()
  "Insert an empty line above the current line."
  (interactive)
  (evim-paste-at (1+ (line-end-position))))

(defvar evim-insert-keymap (make-composed-keymap '(text-mode-map)))
(defvar evim-normal-keymap (make-sparse-keymap))
(defvar evim-visual-keymap (make-sparse-keymap))

(defmacro evim-define-mode (name)
  `(let ((mode-sym ',(intern (concat "evim-" (symbol-name name) "-mode"))))
     (defun ,(intern (concat "evim--" (symbol-name name) "-mode-enable")) nil)
     (defun ,(intern (concat "evim--" (symbol-name name) "-mode-disable")) nil)
     (defun ,(intern (concat "evim-" (symbol-name name) "-mode-enable")) () (interactive) (funcall mode-sym +1))
     (defun ,(intern (concat "evim-" (symbol-name name) "-mode-disable")) () (interactive) (funcall mode-sym -1))
     (define-minor-mode ,(intern (concat "evim-" (symbol-name name) "-mode"))
       ,(concat "Toggle Evim " (symbol-name name) " minor mode.

Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.")
       :init-value nil
       :lighter ,(concat " Evim:" (symbol-name name))
       :keymap ,(intern (concat "evim-" (symbol-name name) "-keymap"))
       :group 'evim
       (let ((mode-sym ',(intern (concat "evim-" (symbol-name name) "-mode")))
             (enable-sym ',(intern (concat "evim--" (symbol-name name) "-mode-enable")))
             (disable-sym ',(intern (concat "evim--" (symbol-name name) "-mode-disable"))))
         (if (symbol-value mode-sym)
             (funcall enable-sym)
           (funcall disable-sym))))))

(defun evim-define-keys (keymaps defs)
  (dolist (def defs)
    (let* ((cmd-keys (nth 0 def))
           (cmd-sym (nth 1 def))
           (cmd-keys (if (stringp cmd-keys)
                         (kbd cmd-keys)
                       cmd-keys)))
      (dolist (keymap keymaps)
        (define-key (symbol-value keymap) cmd-keys cmd-sym)))))

(evim-define-mode insert)
(evim-define-keys
 '(evim-insert-keymap)
 '(
   ("C-a" beginning-of-line)
   ("C-e" end-of-line)
   ("C-f" forward-char)
   ("C-b" backward-char)
   ("C-l" completion-at-point)
   ("C-n" next-line)
   ("C-p" previous-line)
   ("C-s" isearch-forward)
   ("C-v" yank)
   ("C-h" xah-delete-backward-char-or-bracket-text)
   ;; ("M-w" forward-word)
   ;; ("M-b" backward-word)
   ("<return>" newline-and-indent)
   ("<C-[>" (lambda () (interactive) (state-transition 'evim-insert-mode 'evim-normal-mode))))
 )

(defun evim-open-line-below ()
  (interactive)
  (end-of-line)
  (open-line 1)
  (next-line)
  (state-transition 'evim-normal-mode 'evim-insert-mode))

(defun evim-open-line-above ()
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (state-transition 'evim-normal-mode 'evim-insert-mode))

(defun evim-join ()
  (interactive)
  (save-excursion
    (end-of-line)
    (forward-char)
    (join-line)))

(defun evim-A ()
  (interactive)
  (end-of-line)
  (state-transition 'evim-normal-mode 'evim-insert-mode))

(defun evim-a ()
  (interactive)
  (forward-char)
  (state-transition 'evim-normal-mode 'evim-insert-mode))

(defun evim-H ()
  (interactive)
  (move-to-window-line 0))

(defun evim-i ()
  (interactive)
  (state-transition 'evim-normal-mode 'evim-insert-mode))

(defun evim-L ()
  (interactive)
  (move-to-window-line -1))

(defun evim-v ()
  (interactive)
  (state-transition 'evim-normal-mode 'evim-visual-mode))

(defun evim-V ()
  (interactive)
  (save-excursion
    (next-line)
    (beginning-of-line)
    (state-transition 'evim-normal-mode 'evim-visual-mode)))

(defun evim-x ()
  (interactive)
  (delete-char 1))

(defvar evim-lb-keymap (make-sparse-keymap))
(evim-define-keys
 '(evim-lb-keymap)
 `(
   ("(" backward-up-list)
   ("[" beginning-of-defun)
   ))
(defvar evim-rb-keymap (make-sparse-keymap))
(evim-define-keys
 '(evim-rb-keymap)
 `(
   (")" up-list)
   ("]" end-of-defun)))

(defvar evim-g-keymap (make-sparse-keymap))
(evim-define-keys
 '(evim-g-keymap)
 `(
   ("g" beginning-of-buffer)
   ("G" end-of-buffer)))

(defvar evim-z-keymap (make-sparse-keymap))
(evim-define-keys
 '(evim-z-keymap)
 `(
   ("z" recenter)))

(defun evim-replace-char (newc)
  (interactive "sEnter char: ")
  (save-excursion
    (let* ((beg (point))
           (end (1+ (point)))
           (oldc (buffer-substring-no-properties beg end)))
      (replace-string-in-region oldc newc beg end))))

(evim-define-mode normal)

(define-key evim-normal-keymap [control-bracketleft] #'evim-normal-mode)
(defun evim--normal-mode-enable () (setq-local cursor-type t))
(defun evim--insert-mode-enable () (setq-local cursor-type 'bar))
(defun evim--insert-mode-disable () (setq-local cursor-type t))

(evim-define-keys
 '(evim-normal-keymap evim-visual-keymap)
 `(
   ("[" ,evim-lb-keymap)
   ("]" ,evim-rb-keymap)
   ("= =" indent-relative)
   ("= a" sam-indent-all)
   ("0" beginning-of-line)
   ("$" end-of-line)
   ("a" evim-a)
   ("A" evim-A)
   ("b" backward-word)
   ("c" ,evim-cut-keymap)
   ("d" ,evim-delete-keymap)
   ("e" forward-word)
   ("g" ,evim-g-keymap)
   ("G" end-of-buffer)
   ("h" backward-char)
   ("H" evim-H)
   ("i" evim-i)
   ("j" next-line)
   ("k" previous-line)
   ("l" forward-char)
   ("L" evim-L)
   ("M" move-to-window-line-top-bottom)
   ("n" isearch-repeat-forwar)
   ("o" evim-open-line-below)
   ("O" evim-open-line-above)
   ("p" ,evim-paste-keymap)
   ("r" evim-replace-char)
   ("u" undo)
   ("v" evim-v)
   ("V" evim-V)
   ("y" ,evim-yank-keymap)
   ("w" forward-to-word)
   ("c" ,evim-cut-keymap)
   ("x" evim-x)
   ("u" undo)
   ("z" ,evim-z-keymap)
   ))

(defface evim-highlight
  '((t
     :inherit region
     ;; :box (:line-width -3 :style released-button)
     ;; :weight bold
     ))
  "Face for evim highlighting.")

(evim-define-mode visual)
(defvar evim-mark (make-marker))
(defvar evim-region-overlay nil)
(defun evim-set-marker ()
  (interactive)
  (set-marker evim-mark (point)))
(defun evim-highlight-region ()
  (interactive)
  (if evim-region-overlay
      (move-overlay evim-region-overlay (marker-position evim-mark) (point))
    (setq evim-region-overlay (make-overlay (marker-position evim-mark) (point))))
  (overlay-put evim-region-overlay 'face 'evim-highlight))
(defun evim-unhighlight-region ()
  (interactive)
  (delete-overlay evim-region-overlay))
(defun evim--visual-mode-enable ()
  (interactive)
  (evim-set-marker)
  (add-hook 'post-command-hook 'evim-highlight-region))
(defun evim--visual-mode-disable ()
  (interactive)
  (remove-hook 'post-command-hook 'evim-highlight-region)
  (evim-unhighlight-region))
(defun evim-kill-region ()
  (interactive)
  (kill-region (marker-position evim-mark) (point))
  (state-transition 'evim-visual-mode 'evim-normal-mode))
(defun evim-copy-region-as-kill ()
  (interactive)
  (copy-region-as-kill (marker-position evim-mark) (point))
  (state-transition 'evim-visual-mode 'evim-normal-mode))
(defun evim-visual-escape ()
  (interactive)
  (state-transition 'evim-visual-mode 'evim-normal-mode))
(evim-define-keys
 '(evim-visual-keymap)
 '(
   ("<C-[>" evim-visual-escape)
   ("C-g" evim-visual-escape)
   ("d" evim-kill-region)
   ("y" evim-copy-region-as-kill)))

(provide 'evim)
;;; evim.el ends here
