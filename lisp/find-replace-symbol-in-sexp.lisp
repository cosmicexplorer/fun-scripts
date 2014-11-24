;;; testing the power of recursion and sexps in lisp

(defun is-symbol-in-sexp (sexp symbol)
  (if (eq sexp nil)
      nil
      (or (if (listp (car sexp))
              (is-symbol-in-sexp (car sexp) symbol)
              (eq (car sexp) symbol))
          (is-symbol-in-sexp (cdr sexp) symbol))))

(defun num-occurrences-symbol-in-sexp (sexp symbol)
  (cond ((eq sexp symbol)
         1)
        ((eq sexp nil)
         0)
        ((listp sexp)
         (+ (num-occurrences-symbol-in-sexp (car sexp) symbol)
            (num-occurrences-symbol-in-sexp (cdr sexp) symbol)))
        (t
         0)))

;;; clone of subst
;;; this worked first try ahaha
(defun replace-symbol-in-sexp-fn (new-symbol symbol-to-replace sexp)
  (if (eq sexp nil)
      sexp
      (cons
       (if (listp (car sexp))
           (replace-symbol-in-sexp-fn symbol-to-replace new-symbol (car sexp))
           (if (eq (car sexp) symbol-to-replace)
               (setf (car sexp) new-symbol)
               (car sexp)))
       (replace-symbol-in-sexp-fn symbol-to-replace new-symbol (cdr sexp)))))

;;; not working, whatever
;; (defmacro replace-symbol-in-body (new-symbol symbol-to-replace &body body)
;;   (remove-one-outer-paren
;;    (loop for sexp in body
;;       collect `(subst ,new-symbol ,symbol-to-replace ',sexp))))

(defmacro replace-symbol-in-sexp (new-symbol symbol-to-replace sexp)
  `(eval ,`(subst ,new-symbol ,symbol-to-replace ',sexp)))

;;; TODO: learn how to do this for body-forms
