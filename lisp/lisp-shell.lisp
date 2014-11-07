(defun get-command-expansion (command)
  "Resolves bash command to executable pointed to by the PATH. Assumes no
aliases."
  (let ((*readtable* (copy-readtable nil))
        (which-output)
        (which-process (run-program "/usr/bin/which" `(,(string command))
                                    :output :stream)))
    (setf (readtable-case *readtable*) :preserve)
    (when which-process
      (with-open-stream (which-out
                         (process-output which-process))
        (setq which-output (read which-out)))
      (process-close which-process))
    (string which-output)))

(defun turn-list-into-list-of-strings (list)
  (loop for element in list
     append (list (string element)) into final
       finally (return final)))

(defmacro read-shell-as-sexp (sexp &rest)
  `(run-program ,(get-command-expansion
                  (car sexp))
                ,(turn-list-into-list-of-strings (cdr sexp))))

(defmacro readreadreadread (input)
  'input)
