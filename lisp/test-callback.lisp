;; testing callbacks from c code
;;; http://sbcl.org/asdf/Using-asdf-to-load-systems.html
;;; https://common-lisp.net/project/cffi/manual/html_node/Installation.html
(asdf:operate 'asdf:load-op 'cffi)
(use-package 'cffi)

;; (define-foreign-library test-callback
;;   (t (:default "test-callback")))

(define-foreign-library test-callback++
  (t (:default "test-callback++")))

(pushnew #P"./" *foreign-library-directories*
         :test #'equal)

(defctype ch :char)
(defctype in :int)

(defcfun "easy_write" ch (i :int))

(defcfun "get_top" in)

(defcallback get-num ch () (+ 2 34 3))

(defcfun "return_your_return" ch (fp :pointer))

(use-foreign-library test-callback++)

;;; these should work as expected
;; (easy-write 3)
;; (return-your-return (callback get-num))
