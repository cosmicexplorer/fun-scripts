(defun get-command-expansion (command)
  "Resolves bash command to executable pointed to by the PATH. Assumes no
aliases or commands with capital letters."
  (let ((*readtable* (copy-readtable nil))
        (which-output)
        (which-process (run-program "/usr/bin/which"
                                    `(,(string-downcase (string command)))
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
     append (list (string element)) into final
     finally (return final)))

;; (defmacro read-shell-as-sexp (command &rest args)
;;   `(run-program ,(get-command-expansion (string command))
;;                 ,(turn-list-into-list-of-strings args)
;;                 :output *standard-output*))

(defun read-sexp-as-shell (sexp)
  "Read sexp as a shell command. Idea of this function is to read into
case-preserved string first, check if eval errors occur, and then check if eval
errors occur when this is translated into a shell command with this function."
  (run-program
   (get-command-expansion (car sexp))
   (turn-list-into-list-of-strings (cdr sexp))
   :output *standard-output*))
