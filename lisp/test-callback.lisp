;; testing callbacks from c code
(use-package 'cffi)

(define-foreign-library test-callback
  (t (:default "test-callback")))

(pushnew #P"./" *foreign-library-directories*
         :test #'equal)

(defctype ch :char)

(defcfun "easy_write" ch (i :int))

(defcallback get-num ch () (+ 2 34 3))

(defcfun "return_your_return" ch (fp :pointer))

(use-foreign-library test-callback)

;;; these should work as expected
;; (easy-write 3)
;; (return-your-return (callback get-num))
