
(defun emotion-cmd (cmd start-motion end-motion)
  (let ((beg
         (save-excursion
           (when start-motion (funcall start-motion))
           (point)))
        (end (save-excursion
               (when end-motion (funcall end-motion))
               (point))))
    (funcall cmd beg end)))

(defmacro emotion-define-cmd (sym doc-str cmd start-motion end-motion)
  `(progn
     (defun ,sym ()
	   ,doc-str
	   (interactive)
       (emotion-cmd ,cmd ,start-motion ,end-motion))))

(provide 'emotion)
