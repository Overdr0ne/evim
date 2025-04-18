;;; evim-lib.el --- library of emulated vim functions and aliases  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Sam

;; Author: Sam <scmorris.dev@gmail.com>
;; Keywords: lisp

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
(require 'dash)
(require 'cl-macs)
(require 'cl-extra)
(require 'state)
(require 'skey)
(require 'emotion)
(require 'pulse)
(require 'line-mark)

(defmacro evim-define-interface (cmd name prefix)
  `(let ((defs
          '((,prefix ,prefix beginning-of-line (lambda ()
                                                 (move-end-of-line nil)
                                                 (unless (eobp) (forward-char))))
            (,prefix "j" nil next-line)
            (,prefix "k" nil previous-line)
            (,prefix "l" nil forward-char)
            (,prefix "h" nil backward-char)
            (,prefix "w" nil (lambda () (forward-to-word 1)))
            (,prefix "W" nil (lambda () (forward-whitespace 1)))
            (,prefix "b" nil (lambda () (forward-word -1)))
            (,prefix "B" nil (lambda () (forward-whitespace -1)))
            (,prefix "e" nil forward-word)
            (,prefix "M-w" nil forward-sexp)
            (,prefix "M-b" nil backward-sexp)
            (,prefix "i w" backward-word forward-word)
            (,prefix "i o" backward-sexp forward-sexp)))
         (keymap (make-sparse-keymap))
         (kmap-sym ',(intern (concat "evim-" name "-keymap"))))
     (defvar ,(intern (concat "evim-" name "-keymap")))
     (dolist (def defs)
       (let* ((pref (nth 0 def))
              (suff (nth 1 def))
              (start-motion (nth 2 def))
              (end-motion (nth 3 def))
              (cmd-str (concat pref (string-replace " " "" suff)))
              (doc-str (concat "Emulate VIM " cmd-str " command."))
              (cmd-sym (intern (concat "evim-" cmd-str)))
              ;; (cmd-keys (string-join (cl-subseq (split-string suff "") 1 -1) " "))
              (cmd-keys suff)
              )
         (defalias cmd-sym
           (lambda ()
             (interactive)
             (emotion-cmd ',cmd start-motion end-motion))
           doc-str)
         (keymap-set keymap cmd-keys cmd-sym)
         ))
     (set kmap-sym keymap)))

(defun evim--delete (beg end)
  "Delete text from start to end."
  (pulse-momentary-highlight-region beg end)
  (sit-for (+ pulse-delay .1))
  (kill-region beg end))

(evim-define-interface evim--delete "delete" "d")
(emotion-define-cmd evim-D
                    "Emulate VIM D command."
                    #'evim--delete nil
                    (lambda ()
                      (interactive)
                      (move-end-of-line nil)))
(emotion-define-cmd evim-delete-forward-sexp "Kill the following sexp." #'evim--delete nil #'forward-sexp)
(emotion-define-cmd evim-delete-backward-sexp "Kill the preceding sexp." #'evim--delete nil #'backward-sexp)

(defun evim--yank (start end)
  "Save text from START to END position."
  (let ((pulse-flag t)
        (pulse-delay .06))
    (pulse-momentary-highlight-region start end))
  (let ((start start)
        (end end))
    (copy-region-as-kill start end)))

(evim-define-interface evim--yank "yank" "y")
(emotion-define-cmd evim-Y
                    "Emulate VIM Y command."
                    #'evim--yank nil
                    (lambda ()
                      (interactive)
                      (move-end-of-line nil)))
(emotion-define-cmd evim-yank-forward-sexp "Copy the following sexp to the kill-ring." #'evim--yank nil #'forward-sexp)
(emotion-define-cmd evim-yank-backward-sexp "Copy the preceding sexp to the kill-ring." #'evim--yank nil #'backward-sexp)

(defun evim--cut (start end)
  "Cut text from START to END position."
  (evim--delete start end)
  (evim-transition-to 'evim-insert-mode))

(evim-define-interface evim--cut "cut" "c")
(emotion-define-cmd evim-C
                    "Emulate VIM C command."
                    #'evim--cut nil
                    (lambda ()
                      (interactive)
                      (move-end-of-line nil)))

(defun evim-visual-cut ()
  (interactive)
  (evim--delete (point) (mark))
  (deactivate-mark t)
  (evim-transition-to 'evim-insert-mode))
(emotion-define-cmd evim-cut-forward-sexp "Kill the following sexp." #'evim--cut nil #'forward-sexp)
(emotion-define-cmd evim-cut-backward-sexp "Kill the preceding sexp." #'evim--cut nil #'backward-sexp)

(defun evim-paste-at (pos)
  (save-excursion
    (goto-char pos)
    (insert (current-kill 0))))

(defun evim--paste (_ pos)
  (save-excursion
    (goto-char pos)
    (insert (current-kill 0))))

(evim-define-interface evim--paste "paste" "p")
(emotion-define-cmd evim-P "Emulate VIM P command." #'evim--paste nil #'beginning-of-line)

(defun evim-pp ()
  (interactive)
  (evim-paste-at (point)))

(defun evim-pP ()
  (interactive)
  (evim-paste-at (point)))

(defun evim-insert-line-below ()
  "Insert an empty line below the current line."
  (interactive)
  (save-excursion
    (end-of-line)
    (open-line 1)))

(defun evim-insert-line-above ()
  "Insert an empty line above the current line."
  (interactive)
  (save-excursion
    (end-of-line 0)
    (open-line 1)))

(defun evim-pk ()
  "Insert an empty line above the current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (not (string= (substring-no-properties (current-kill 0) -1) "\n"))
      (open-line 1))
    (evim-paste-at (point))
    )
  )

(defun evim-pj ()
  "Insert an empty line above the current line."
  (interactive)
  (save-excursion
    (next-line)
    (beginning-of-line)
    (when (not (string= (substring-no-properties (current-kill 0) -1) "\n"))
      (open-line 1))
    (evim-paste-at (point))))

(defun evim-open-line-below ()
  (interactive)
  (end-of-line)
  (newline-and-indent)
  (evim-transition-to 'evim-insert-mode))

(defun evim-open-line-above ()
  (interactive)
  (beginning-of-line)
  (save-excursion (insert "\n"))
  (indent-according-to-mode)
  (evim-transition-to 'evim-insert-mode))

(defun evim-join ()
  (interactive)
  (save-excursion
    (end-of-line)
    (forward-char)
    (join-line)))

(defun evim-A ()
  (interactive)
  (end-of-line)
  (evim-transition-to 'evim-insert-mode))

(defun evim-a ()
  (interactive)
  (forward-char)
  (evim-transition-to 'evim-insert-mode))

(defun evim-B ()
  "Move backward previous WORD."
  (interactive)
  (backward-char)
  (forward-whitespace -1)
  (forward-whitespace +1))

(defun evim-H ()
  (interactive)
  (move-to-window-line 0))

(defun evim-i ()
  (interactive)
  (evim-transition-to 'evim-insert-mode))

(defun evim-I ()
  (interactive)
  (end-of-line)
  (evim-transition-to 'evim-insert-mode))

(defun evim-L ()
  (interactive)
  (move-to-window-line -1))

(defun evim-V ()
  (interactive)
  (save-excursion
    (next-line)
    (beginning-of-line)
    (evim-transition-to 'evim-visual-mode)))

(defun evim-x ()
  (interactive)
  (delete-char 1))

(defun evim-replace-char (newc)
  (interactive "cEnter char: ")
  (save-excursion
    (let* ((beg (point))
           (end (1+ (point)))
           (oldc (buffer-substring-no-properties beg end)))
      (replace-string-in-region oldc newc beg end))))

(defun evim-replace-region-with-kill ()
  (interactive)
  (delete-region (mark) (point))
  (evim-transition-to 'evim-normal-mode)
  (yank))

(defun evim-indent ()
  (interactive)
  (indent-according-to-mode))

(defvar evim-lb-keymap (make-sparse-keymap))
(defun backward-end-of-defun ()
  (interactive)
  (end-of-defun -1))
(skey-define-keys
 '(evim-lb-keymap)
 `(
   ("(" backward-up-list)
   ("[" beginning-of-defun)
   ("]" backward-end-of-defun)
   ))
(defvar evim-rb-keymap (make-sparse-keymap))
(defun forward-beginning-of-defun ()
  (interactive)
  (beginning-of-defun -1))
(skey-define-keys
 '(evim-rb-keymap)
 `(
   (")" up-list)
   ("[" forward-beginning-of-defun)
   ("]" end-of-defun)
   ))

(defvar evim-g-keymap (make-sparse-keymap))
(skey-define-keys
 '(evim-g-keymap)
 `(
   ("f" find-file-at-point)
   ("g" beginning-of-buffer)
   ("G" end-of-buffer)))

(defvar evim-z-keymap (make-sparse-keymap))
(skey-define-keys
 '(evim-z-keymap)
 `(
   ("z" recenter)))

(defvar evim-hjkl-keymap (make-sparse-keymap))
(skey-define-keys
 '(evim-hjkl-keymap)
 '(
   ("h" backward-char)
   ("j" next-line)
   ("k" previous-line)
   ("l" forward-char)
   ))

(defun evim-kmacro-insert-last ()
  (interactive)
  (kmacro-start-macro-or-insert-counter ))

(defvar evim-kmacro-keymap (make-sparse-keymap))
(skey-define-keys
 '(evim-kmacro-keymap)
 '(
   ("s" kmacro-start-macro-or-insert-counter)
   ("e" kmacro-end-or-call-macro)
   ("i" kmacro-insert-counter)
   ("q" consult-kmacro)
   ))

(defun evim-indent-line-left ()
  (interactive)
  (let* ((beg (line-beginning-position))
         (end (line-end-position)))
    (indent-rigidly-left beg end)))

(defun evim-indent-line-right ()
  (interactive)
  (let* ((beg (line-beginning-position))
         (end (line-end-position)))
    (indent-rigidly-right beg end)))

(defvar-local evim--current-mode nil)
(defmacro evim-define-mode (name)
  `(progn
     (define-minor-mode ,(intern (concat "evim-" (symbol-name name) "-mode"))
       ,(concat "Toggle Evim " (symbol-name name) " minor mode.

Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.")
       :init-value nil
       :lighter ,(concat " Evim:" (symbol-name name))
       :keymap (make-sparse-keymap)
       :group 'evim
       (if ,(intern (concat "evim-" (symbol-name name) "-mode"))
           (setq evim--current-mode ',(intern (concat "evim-" (symbol-name name) "-mode")))))))

(defmacro evim-define-derived-mode (child parent)
  `(progn
     (define-minor-mode ,(intern (concat "evim-" (symbol-name parent) "-" (symbol-name child) "-mode"))
       ,(concat "Toggle Evim " (symbol-name child) " derived from " (symbol-name parent) " minor mode.

Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.")
       :lighter ,(concat " Evim:" (symbol-name parent) ":" (symbol-name child))
       :keymap (let ((map (make-sparse-keymap)))
                 (set-keymap-parent map ,(intern (concat "evim-" (symbol-name parent) "-mode-map")))
                 map)
       :group 'evim
       (if ,(intern (concat "evim-" (symbol-name parent) "-" (symbol-name child) "-mode"))
           (progn
             (run-hooks ',(intern (concat "evim-" (symbol-name parent) "-mode-on-hook")))
             (setq evim--current-mode ',(intern (concat "evim-" (symbol-name parent) "-" (symbol-name child) "-mode"))))
         (run-hooks ',(intern (concat "evim-" (symbol-name parent) "-mode-off-hook")))))))

(defun evim-define-default-derived-modes (child)
  (eval
   `(progn
      (evim-define-derived-mode ,child normal)
      (evim-define-derived-mode ,child visual)
      (evim-define-derived-mode ,child insert)
      ;; Setup all the weird hooks necessary to automatically switch to/from visual mode
      ;; when the mark is activated/deactivated
      (defun ,(intern (concat "evim-" (symbol-name child) "-escape")) ()
        (interactive)
        (evim-transition-to ',(intern (concat "evim-normal-" (symbol-name child) "-mode"))))
      (defun ,(intern (concat "evim-" (symbol-name child) "--activate-mark")) ()
        (evim-transition-to ',(intern (concat "evim-visual-" (symbol-name child) "-mode"))))
      (defun ,(intern (concat "evim-" (symbol-name child) "--deactivate-mark")) ()
        (,(intern (concat "evim-" (symbol-name child) "-escape"))))
      (defun ,(intern (concat "evim-" (symbol-name child) "--normal-mode-enable")) ()
        (setq-local cursor-type t)
        ;; TODO: find a cleaner way to add these hooks only when
        ;; when a given evim mode is being used.
        (evim-normal-mode -1)
        (remove-hook 'activate-mark-hook #'evim--activate-mark t)
        (remove-hook 'deactivate-mark-hook #'evim--deactivate-mark t)
        (add-hook 'activate-mark-hook
                  #',(intern (concat "evim-" (symbol-name child) "--activate-mark")) 0 t)
        (add-hook 'deactivate-mark-hook
                  #',(intern (concat "evim-" (symbol-name child) "--deactivate-mark")) 0 t))
      (add-hook ',(intern (concat "evim-normal-" (symbol-name child) "-mode-on-hook"))
                #',(intern (concat "evim-" (symbol-name child) "--normal-mode-enable")))

      (defun ,(intern (concat "evim-" (symbol-name child) "-A")) ()
        (interactive)
        (end-of-line)
        (evim-transition-to ',(intern (concat "evim-insert-" (symbol-name child) "-mode"))))

      (defun ,(intern (concat "evim-" (symbol-name child) "-a")) ()
        (interactive)
        (forward-char)
        (evim-transition-to ',(intern (concat "evim-insert-" (symbol-name child) "-mode"))))

      (defun ,(intern (concat "evim-" (symbol-name child) "-i")) ()
        (interactive)
        (evim-transition-to ',(intern (concat "evim-insert-" (symbol-name child) "-mode"))))

      (skey-define-keys
       (list ',(intern (concat "evim-normal-" (symbol-name child) "-mode-map")))
       (list
        (list "a" ',(intern (concat "evim-" (symbol-name child) "-a")))
        (list "A" ',(intern (concat "evim-" (symbol-name child) "-A")))
        (list "i" ',(intern (concat "evim-" (symbol-name child) "-i")))))

      (skey-define-keys
       (list ',(intern (concat "evim-insert-" (symbol-name child) "-mode-map")))
       (list
        (list "<remap> <evim-escape>" ',(intern (concat "evim-" (symbol-name child) "-escape")))
        ))
      )))

(defun evim-transition-to (target-mode)
  "Transition from evim--curent-mode to TARGET-MODE."
  (state-transition evim--current-mode target-mode))

(defun evim-escape ()
  (interactive)
  (evim-transition-to 'evim-normal-mode))

(evim-define-mode insert)

(defun evim--insert-mode-enable ()
  (setq-local cursor-type 'bar))
(add-hook 'evim-insert-mode-on-hook #'evim--insert-mode-enable)

(skey-define-keys
 '(evim-insert-mode-map)
 `(
   (";" self-insert-command)
   ("?" self-insert-command)
   ("SPC" self-insert-command)
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
   ("<return>" newline-and-indent)
   ("<escape>" evim-escape)
   ("M-x" execute-extended-command)
   )
 )

(evim-define-mode normal)

(defun evim--activate-mark ()
  (evim-transition-to 'evim-visual-mode))
(defun evim--deactivate-mark ()
  (evim-escape))
(defun evim--normal-mode-enable ()
  (setq-local cursor-type t)
  ;; TODO: find a cleaner way to add these hooks only when
  ;; when a given evim mode is being used.
  (add-hook 'activate-mark-hook #'evim--activate-mark 0 t)
  (add-hook 'deactivate-mark-hook #'evim--deactivate-mark 0 t))
(add-hook 'evim-normal-mode-on-hook #'evim--normal-mode-enable)

(skey-define-keys
 '(evim-normal-mode-map)
 `(
   ("= =" evim-indent)
   ("= a" sam-indent-all)
   ("<" evim-indent-line-left)
   (">" evim-indent-line-right)
   ))

(evim-define-mode visual)
(skey-define-keys
 '(evim-normal-mode-map evim-visual-mode-map)
 `(
   ("1" digit-argument)
   ("2" digit-argument)
   ("3" digit-argument)
   ("4" digit-argument)
   ("5" digit-argument)
   ("6" digit-argument)
   ("7" digit-argument)
   ("8" digit-argument)
   ("9" digit-argument)
   ("0" beginning-of-line)
   ("!" nil)
   ("@" nil)
   ("#" nil)
   ("$" end-of-line)
   ("%" nil)
   ("^" nil)
   ("&" nil)
   ("*" nil)
   ("(" nil)
   (")" nil)
   ("-" previous-line)
   ("+" next-line)
   ("/" comment-line)
   ("[" ,evim-lb-keymap)
   ("]" ,evim-rb-keymap)
   ("a" evim-a)
   ("A" evim-A)
   ("b" backward-word)
   ("B" evim-B)
   ("c" ,evim-cut-keymap)
   ("M-c" evim-cut-forward-sexp)
   ("M-C" evim-cut-backward-sexp)
   ("C" nil)
   ("d" ,evim-delete-keymap)
   ("D" evim-D)
   ("M-d" evim-delete-forward-sexp)
   ("M-D" evim-delete-backward-sexp)
   ("e" forward-word)
   ("E" end-of-line)
   ("f" nil)
   ("F" nil)
   ("g" ,evim-g-keymap)
   ("G" end-of-buffer)
   ("h" backward-char)
   ("H" evim-H)
   ("i" evim-i)
   ("I" evim-I)
   ("j" next-line)
   ("J" evim-join)
   ("k" previous-line)
   ("K" help)
   ("l" forward-char)
   ("L" evim-L)
   ("m" point-to-register)
   ("M" move-to-window-line-top-bottom)
   ("n" isearch-repeat-forward)
   ("N" isearch-repeat-backward)
   ("o" evim-open-line-below)
   ("O" evim-open-line-above)
   ("p" ,evim-paste-keymap)
   ("P" evim-P)
   ("q" ,evim-kmacro-keymap)
   ("C-q" quit-window)
   ("r" evim-replace-char)
   ("R" overwrite-mode)
   ("s" nil)
   ("S" nil)
   ("u" undo)
   ("U" undo-redo)
   ("v" set-mark-command)
   ("C-v" rectangle-mark-mode)
   ("V" line-mark-mode-activate)
   ("w" forward-to-word)
   ("W" forward-whitespace)
   ("x" evim-x)
   ("X" nil)
   ("y" ,evim-yank-keymap)
   ("Y" evim-Y)
   ("M-y" evim-yank-forward-sexp)
   ("M-Y" evim-yank-backward-sexp)
   ("z" ,evim-z-keymap)))

(keymap-set evim-normal-mode-map "d" nil)
(keymap-set evim-normal-mode-map "d" evim-delete-keymap)

(defun evim--visual-mode-enable ()
  (setq-local cursor-type 'bar))
(add-hook 'evim-visual-mode-on-hook #'evim--visual-mode-enable)

(skey-define-keys
 '(evim-visual-mode-map)
 '(
   ("M-x" execute-extended-command)
   ("=" indent-region)
   (">" indent-rigidly-right)
   ("<" indent-rigidly-left)
   ("/" comment-or-uncomment-region)
   ("<escape>" keyboard-quit)
   ("c" evim-visual-cut)
   ("d" kill-region)
   ("p" evim-replace-region-with-kill)
   ("s" isearch-forward)
   ("v" er/expand-region)
   ("y" copy-region-as-kill)))

(defcustom evim-major-mode-hooks
  '( diff-mode-hook text-mode-hook prog-mode-hook conf-mode-hook Info-mode-hook helpful-mode-hook bitbake-mode-hook extempore-mode-hook)
  "Major mode hooks to initialize the evim state machine")

(defun global-evim-mode--init ()

  (dolist (hook evim-major-mode-hooks)
    (add-hook hook #'evim-normal-mode))
  t)
(defun global-evim-mode--shutdown ()
  (dolist (hook evim-major-mode-hooks)
    (remove-hook hook #'evim-normal-mode))
  nil)
(defvar global-evim-mode nil
  "Contains global data for global-evim-mode.")
;;;###autoload
(define-minor-mode global-evim-mode
  "Toggle global evim minor mode.

Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When enabled, adds hooks to common major mode sets to begin editing
in the evim-normal-mode state."
  :init-value nil
  :lighter " Evim:global"
  :group 'evim
  :global t
  (if global-evim-mode
      (setf global-evim-mode (global-evim-mode--init))
    (setf global-evim-mode (global-evim-mode--shutdown))))

(provide 'evim-lib)
;;; evim-lib.el ends here
