(defun make-list-with-append (given-list)
  "Takes list, returns list of two values: given list (car) and final node of given list (cdr).
This structure can be used to append to the end of the given list in constant time."
  (cons given-list (last given-list))) ; linear-time traversal, now cached

(defun append-to-list-with-append-struct (list-with-append object-to-append)
  "Takes atom or list to append to list, append at end, updating the cached final node."
  (if (atom object-to-append)
      ;; if atomic
      (rplacd list-with-append (cdr (rplacd (cdr list-with-append) (cons object-to-append nil))))
      ;; if list
      (rplacd list-with-append (last (rplacd (cdr list-with-append) object-to-append)))))
