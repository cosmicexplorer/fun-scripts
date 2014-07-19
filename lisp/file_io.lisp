;;; various file io operations

(defconstant +BLOCK-SIZE+ 8192)

(defun display-file)

(let ((infile (open "file_io.lisp" :if-does-not-exist nil)))
  (if infile
      (loop for active-byte = (read-byte infile nil :eof)
         until (eq active-byte :eof)
         do (format t "~a" active-byte))
      (close infile)
      ))


