(defpackage :package-testing
  (:use :common-lisp)
  (:export :load-so
           :print-text
           :call-print-text-ffi))

(defun load-so ()
  (load-shared-object "/home/cosmicexplorer/snippets/lisp/test_ffi_lib.so"))

(defun call-print-text-ffi (argv)
    (let ((in-string "hiya") (out-string (make-alien char 20)))
      (load-so)
      (define-alien-routine print_text unsigned-long ; size_t is typedef to unsigned long
        (in-string (c-string :external-format :ascii))
        (out-string (c-string :external-format :ascii)))
      (format t "~a~%" (print_text in-string out-string))
      (format t "~a~%" in-string)
      (with-alien ((conv-out-string (c-string :external-format :ascii) out-string))
        (format t "~a~%" (subseq conv-out-string 0 20)))))
;; won't parse, needs the above with-alien
;; (format t "~a~%" out-string)

;;; ffi through lisp matches performance of native c!

;; lisp
;; 7
;; text!!!
;; text!text!text!
;; ./test_ffi_lisp_exe  246.64s user 0.05s system 100% cpu 4:06.61 total
;; c
;; 4
;; hiya
;; ./test_ffi_c_exe  245.68s user 0.00s system 100% cpu 4:05.57 total

