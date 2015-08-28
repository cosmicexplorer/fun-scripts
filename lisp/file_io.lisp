;;; various file io operations

;; (defun test (func arg &rest args)
;;   (cond ((atom args)
;;          (test func arg))
;;         ((atom (cdr args))
;;          (test func (cons arg (car args))))
;;         (t
;;          (print 1))))
;; (test #'1+ 2)
;; (test 2)
;; (test '(2))

(defconstant +BLOCK-SIZE+ 8192)         ; size of block to read in
(defconstant +CODON-LENGTH+ 3)          ; length of codon in bases

(define-condition file-not-found-error (error)
  ((error-text :initarg :error-text :reader get-error-text)))

(defun vcsfmt-open-file (filename-str)
  "Opens file, if possible. Throws error if file does not exist."
  ;; element-type CHARACTER
  (let ((input-file (open filename-str
                          :if-does-not-exist nil
                          :element-type 'base-char)))
    (if input-file
        input-file
        (error 'file-not-found-error :error-text "Input file not found."))))

(defun vcsfmt-initialize-char-sequences ()
  "Returns input and output char sequences for file I/O.
Sequences are strings (char sequences) and are of length specialized to DNA."
  ;; sequences are of element type CHARACTER to avoid casting from file stream
  (values (make-sequence 'string +BLOCK-SIZE+) ; seq to read into
          ;; since it takes at least two codons (6 bases) to make an ORF,
          ;; and typically only a newline is inserted between ORFs,
          ;; this is the longest conceivable output sequence
          (make-sequence 'string (+ +BLOCK-SIZE+ ; seq to write out
                                    (round (/ +BLOCK-SIZE+
                                              (* 2 +CODON-LENGTH+)))))
          (make-sequence 'string +CODON-LENGTH+))) ; seq to contain current codon

(defvar *input-seq* nil)
(defvar *output-seq* nil)
(defvar *codon-seq* nil)

(defun display-file (filename-str)
  "Opens file and reads from it in blocks, printing to stdout.
Closes file at end. Prints error and returns nil if file not found, t if found
and successfully completed."
  (let (input-file input-seq output-seq codon-seq)
    (handler-case
        (setf input-file (vcsfmt-open-file filename-str))
      (file-not-found-error (err) (format t "~a" (get-error-text err))))
    (if input-file                      ; nil if error thrown
        (progn
          (multiple-value-bind (inseq outseq codseq)
              (vcsfmt-initialize-char-sequences)
            (setf input-seq inseq
                  output-seq outseq
                  codon-seq codseq))
          (loop for bytes-read = (read-sequence input-seq input-file)
             until (= bytes-read 0)
             do (format t "~a" (subseq input-seq 0 bytes-read)))
          (close input-file))
        nil)))

(defvar *is-first-codon* t)
(defvar *bytes-in* 0)
(defvar *bytes-out* 0)

(defun vcsfmt-process-block ()
  "Takes input nucleotide sequence and performs all transformations on it,
copying to output sequence. Precondition: output sequence must be long enough
to handle added characters. codon-seq (3 chars) used to store current codon.
Produces undefined behavior if is-first-codon or bytes-to-process are
incorrectly set."
  (map nil #'vcsfmt-process-char *input-seq*)
  )

;; (declaim (inline vcsfmt-process-char))
;; (defun vcsfmt-process-char (nucleotide)
;;   "Add any additional formatting to output char sequence as required.
;; As of now, just adds newlines between ORFs."
;;   (declare (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0)))
;;   (declare (character nucleotide) (string output-seq))
;;   )
