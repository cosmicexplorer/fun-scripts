;; buildapp --load test.lisp \
;; --entry main \
;; --output test_loop_exe

;; TODO: make things non-global? idk i kinda like it it's easy to change things

(defconstant +prompt-str+ "CL-LIVE> ")
;; change to (concatenate 'string (princ-to-string #\return) (princ-to-string #\linefeed)) in windows
(defconstant +line-ending+ (princ-to-string #\linefeed))

;; example of background function with progress that can be queried within homegrown repl
;;; not intended to be useful otherwise
(defvar *constantly-incrementing* 0)
(defun increment-every-quarter-second ()
	(setq *constantly-incrementing* 0)
	(loop ;; forever
		 (sleep (/ 1 4))
		 (setq *constantly-incrementing* (1+ *constantly-incrementing*))))
(defun get-current-increment-value ()
	*constantly-incrementing*)
(defun launch-repl ()
  (format t (concatenate 'string
												 (format nil "~a" "REPL loaded. Begin typing!")
                         (princ-to-string +line-ending+)
                         (format nil +prompt-str+)))
  (loop
     (format t (concatenate 'string
                            (format nil "~S"
                                    (restart-case
                                        (eval (read-and-add-to-history-list))
                                      ;; provide restart option to continue onward
                                      (continue-as-normal ()
                                        :report "Continue to top level.")))
                            +line-ending+
                            +prompt-str+))))


(defvar *history-string-list* nil)
(defvar *history-string-list-current-position* nil)
;;; TODO: make key sequence to automatically print to line,
;;; then somehow add that string to the sequence read so that it can be manipulated and read
;;; as if the user had typed it in themselves
(defun read-and-add-to-history-list ()	; returns multiple values: sexp, and length of string
	(let ((input-str (format nil "~s" (read-preserving-whitespace))))
		(setf *history-string-list* (cons input-str *history-string-list*))	; set position back to zero
		(read-from-string input-str)))
(defun get-previous-history-list-element ()	; returns string and moves back one element
	(car (setf *history-string-list-current-position* (cdr *history-string-list-current-position*))))
(defun get-following-history-list-element () ; find doubly linked list (or make one lol)
	)
(defun reset-previous-history-list-element ()
	;; need way to escape calls to format; right now they force entering debugger when list is shown
	(setf *history-string-list-current-position* *history-string-list*))

;; paul graham's homegrown repl which additionally exits at :q (from "On Lisp")
;; (defun break-loop (fn quit &rest args)
;;   (format *query-io* "Entering break-loop.~%")
;;   (loop
;;      (let ((in (apply #'prompt args)))
;;        (if (funcall quit in)
;;            (return)
;;            (format *query-io* "~A~%" (funcall fn in))))))
;; (defun prompt (&rest args)
;;   (apply #'format *query-io* args)
;;   (read *query-io*))
;; (break-loop #'eval #'(lambda (x) (eq x :q)) ">> ") ; run loop

(defun main (argv)
  (declare (ignore argv))
  (sb-thread:make-thread #'increment-every-quarter-second) ; launches in background
  (launch-repl))
