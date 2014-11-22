(defun get-command-expansion (command)
  "Resolves bash command to executable pointed to by the PATH. Assumes no
aliases or commands with capital letters."
  (let ((*readtable* (copy-readtable nil))
        (which-output)
        (which-process (run-program "/usr/bin/which"
                                    ;; string-downcase not required if command
                                    ;; is capitalized correctly
                                    `(,(string-downcase (symbol-name command)))
                                    :output :stream)))
    (setf (readtable-case *readtable*) :preserve)
    (when which-process
      (with-open-stream (which-out
                         (process-output which-process))
        (setq which-output (read-preserving-whitespace which-out)))
      (process-close which-process))
    (string which-output)))

(defun turn-list-into-list-of-strings (list)
  (loop for element in list
     append (list (symbol-name element)) into final
     finally (return final)))

;; (defmacro read-shell-as-sexp (command &rest args)
;;   `(run-program ,(get-command-expansion (string command))
;;                 ,(turn-list-into-list-of-strings args)
;;                 :output *standard-output*))

;;; TODO: need method of parsing arbitrary argument to string

(defun read-sexp-as-shell (sexp)
  "Read sexp as a shell command. Idea of this function is to read into
case-preserved string first, check if eval errors occur on the upcased version,
and then check if eval errors occur when this string is translated into a shell
command with read-from-string into this function."
  (run-program
   (get-command-expansion (car sexp))
   (turn-list-into-list-of-strings (cdr sexp))
   :output *standard-output*))
