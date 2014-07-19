;;; a quick way to make a list with constant-time appending to end
;; mostly just playing around with simple data structures in lisp

(defun make-list-with-append (&optional given-list)
  "Takes list, returns list of two values: given list (car) and final node of given list (cdr).
This structure can be used to append to the end of the given list in constant time. O(n) complexity."
  (if (> (length given-list) 0)         ; if list or empty parens given
      (cons given-list (last given-list)) ; linear-time traversal, now cached
      (cons nil nil)))

;;; this could have been done without the double branching using setf whether a list has elements or not
;;; but that would have required a cons or two each time it is run, which is troublesome at scale
;;; branch predictions makes this not a huge speed loss
(defun append-to-end-of-list-with-append (list-with-append object-to-append)
  "Takes atom or list to append to list, append at end, updating the cached final node. O(1) complexity."
  (if (car list-with-append)            ; if list non-nil
      (if (atom object-to-append)
          ;; if atomic
          (rplacd list-with-append (cdr (rplacd (cdr list-with-append) (cons object-to-append nil))))
          ;; if list (linear-time traversal of appended list)
          (rplacd list-with-append (last (rplacd (cdr list-with-append) object-to-append))))
      (if (atom object-to-append)       ; if list empty
          ;; if atomic
          (let ((allocated-object (cons object-to-append nil))) ; cached so that both nodes point to same place
            (setf list-with-append (cons allocated-object allocated-object)))
          ;; if list
          (setf list-with-append (cons object-to-append object-to-append)))))

(defun unappend-from-end-of-list-with-append (list-with-append &optional (number-to-remove 1))
  "Removes elements from end of list and updates cached end node. O(n) complexity."
  (if (and (car list-with-append)
           (>= number-to-remove 1))             ; if non-empty and number-to-remove well-formed
      (let ((last-n-nodes (last (car list-with-append) (+ number-to-remove 1)))) ; computational expense
        (if (<= (length last-n-nodes) number-to-remove)
            (rplaca (rplacd list-with-append nil) nil) ; return empty
            (rplacd list-with-append (rplacd last-n-nodes nil)))) ; nullify final node, change cached end
      ;; return list unaltered if empty
      list-with-append))

;;; retrieve actual list data from low-level structure
(defun get-list (list-with-append)
  "Returns list within structure."
  (car list-with-append))
(defun get-last-node (list-with-append)
  (cdr list-with-append))
