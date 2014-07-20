;; buildapp --load test.lisp \
;; --entry main \
;; --output test_loop_exe

(defconstant +prompt-str+ "CL-LIVE> ")
(defconstant +line-ending+ #\linefeed)

(defvar *constantly-incrementing* 0)

(defun increment-every-quarter-second ()
  (setq *constantly-incrementing* 0)
  (loop
     (sleep (/ 1 4))
     (setq *constantly-incrementing* (1+ *constantly-incrementing*))))

(defun launch-repl ()
  (format t (concatenate 'string (format nil "~a" "REPL loaded. Begin typing!")
                         (princ-to-string +line-ending+)
                         (format nil +prompt-str+)))
  (loop
     (format t (concatenate 'string
                            (format nil "~S"
                                    (restart-case
                                        (eval (read-preserving-whitespace))
                                      (continue-as-normal () ; stop the more annoying errors
                                        :report "Continue to top level.")))
                            (princ-to-string +line-ending+)
                            +prompt-str+))))

(defun main (argv)
  (declare (ignore argv))
  (sb-thread:make-thread #'increment-every-quarter-second) ; launches in background
  (launch-repl))
