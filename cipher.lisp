(in-package :cl-user)
(defpackage lisp-cipher
  (:use :cl :alexandria :serapeum)
  (:export
   :expt-mod
   :caesar-cipher-char
   :caesar-cipher
   :make-playfair
   :des))
(in-package :lisp-cipher)

(defmacro multiple-value-setf (places value-form)
  "Bind PLACES to values returned from VALUE-FORM using setf.
If there are more PLACES than values from VALUE-FORM, nil is assigned to the extra PLACES.
If there are more values than PLACES, the extra values are discarded."
  `(setf (values ,@places)
         ,value-form))

(defun expt-mod (base exponent modulus)
  "Efficiently raise BASE to the power EXPONENT, modulo MODULUS."
  (declare (optimize (speed 3))
           (type fixnum base exponent modulus))
  (loop with result fixnum = 1
        for i fixnum from 0 below (integer-length exponent) 
        for sqr fixnum = base then (mod (* sqr sqr) modulus)
        when (logbitp i exponent)
          do (setf result (mod (* result sqr) modulus))
        finally (return result)))

(defun euclid (dividend divisor)
  "Computes the extended Euclidian algorithm for DIVIDEND/DIVISOR.
Returns values: Bezout coefficients S and T, the gcd R, and quotients DIVIDEND/R and DIVISOR/R.
See Wikipedia for more details, as well as the pseudocode used:
https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm."

  (loop with (old_r new_r) = (list dividend divisor)
        with (old_s new_s) = (list 1 0)
        with (old_t new_t) = (list 0 1)

        while (not (zerop new_r))
        for quotient = (truncate old_r new_r)
        do (psetf
            new_r (- old_r (* quotient new_r))
            old_r new_r

            new_s (- old_s (* quotient new_s))
            old_s new_s

            new_t (- old_t (* quotient new_t))
            old_t new_t)
        finally (return (values old_s old_t old_r new_t new_s))))

(defun euclid-inverse (a n)
  "Computes the modular inverse of A modulo N."
  (multiple-value-bind (coef_s coef_t r)
      (euclid n a)
    (declare (ignore coef_s))
    (unless (> r 1)
      (if (< coef_t 0)
          (+ coef_t n)
          coef_t))))

(defun caesar-cipher-char (char &optional (shift 3) (ignore-foreign t))
  (if (not (alpha-char-p char))
      (if ignore-foreign
          char
          (error "Character outside caesar cipher range!"))
      (code-char (let* ((i (char-code char))
                        (shifted (mod (+ i shift) (char-code #\z))))
                   (if (< shifted (char-code #\a))
                       (incf shifted (1- (char-code #\a)))
                       shifted)))))


(defun caesar-cipher (message &optional (shift 3))
  "Cipher MESSAGE with the caesar-cipher"
  (let ((cipher (make-array (length message) :element-type 'character)))
    (loop for i from 0 below (length message)
          for char = (elt message i)
          do (setf (elt cipher i) (caesar-cipher-char char shift)))
    cipher))

(defun playfair-sanitize-key (text)
  "Sanitize key for use in the playfair cipher"
  (sanitize (remove-duplicates text :from-end t)))

(defun make-playfair-square (key)
  "Constructs a new playfair square using KEY.
As per the original algorithm, #\j is removed from the key."
  (if (every #'alpha-char-p key)
      (let* ((backing-vector
               (make-array 25 :element-type 'character
                              :initial-contents
                              (remove #\j
                                      (append (coerce (remove-duplicates key) 'list)
                                              (loop for code from (char-code #\a)
                                                      upto (char-code #\z)
                                                    for char = (code-char code)
                                                    when (not (find char key))
                                                      collect char))))))
        (values 
         (make-array '(5 5)
                     :element-type 'character
                     :displaced-to backing-vector)
         backing-vector))
      (error "Key contains non-alphanumeric characters!")))

(defun playfair-sanitize-message (message)
  "Prepare a message for use in the playfair cipher."
  (with-output-to-string (stream)
    (let ((message
            (substitute #\i #\j
                        (sanitize message))))
      (loop for i from 0 below (1- (length message))
            for d1 = (elt message i)
            for d2 = (elt message (incf i))
            do (progn (write-char d1 stream)
                      (if (char= d1 d2)
                          (progn (decf i)
                                 (write-char #\x stream))
                          (write-char d2 stream)))
            finally (when (< i (length message))
                      (write-char (elt message (1- (length message))) stream)
                      (write-char #\x stream))))))

(defun make-playfair (key)
  "Return a closure which calculates the Playfair cipher using KEY."
  (multiple-value-bind (key-square backing-vector) (make-playfair-square
                                                    (playfair-sanitize-key key))
    (lambda (message &optional (enc t))
      
      (let* ((plaintext (playfair-sanitize-message message))
             (ciphered (make-array (length plaintext) :element-type 'character)))
        (loop for i from 0 below (1- (length plaintext))
              for d1 = (elt plaintext i)
              for d2 = (elt plaintext (1+ i))

              do
                 (setf (values (elt ciphered i) (elt ciphered (1+ i)))
                       (playfair-cipher-digraph d1 d2 key-square backing-vector enc)
                       i (1+ i))
              finally (return ciphered))))))

(defun playfair-cipher-digraph (d1 d2 key-square backing-vector enc
                                &aux (p1 (array-index-row-major key-square (position d1 backing-vector)))
                                  (p2 (array-index-row-major key-square (position d2 backing-vector))))
  "Translate a digraph in the playfair cipher to "
  (declare (type character d1 d2))
  (flet ((same-column ()
           (= (second p1)
              (second p2)))
         (same-row ()
           (= (first p1)
              (first p2)))
         (linear-shift (i)
           (mod (if enc
                    (1+ i)
                    (1- i))
                5)))
    (cond
      ((same-row) (values (aref key-square (first p1) (linear-shift (second p1)))
                          (aref key-square (first p2) (linear-shift (second p2)))))
      ((same-column) (values (aref key-square (linear-shift (first p1)) (second p1))
                             (aref key-square (linear-shift (first p2)) (second p2))))
      (t
       (values
        (aref key-square (first p1) (second p2))
        (aref key-square (first p2) (second p1)))))))

(defun perfect-square-p (n)
  (let ((sq (isqrt n)))
    (when (and (>= n 0)
               (= n (expt sq 2)))
      sq)))

(defun sanitize (text)
  "Clean up text for use in any cipher"
  (string-downcase
   (remove-if-not
    #'alpha-char-p
    text)))

(defun mini-char-code (character)
  (- (char-code character) (char-code #\a)))

(defun mini-code-char (code)
  (code-char (+ code
                (char-code #\a ))))

(defun hill-key (key)
  "Create a hill key matrix from KEY"
  (let ((key-backer (map 'vector #'mini-char-code
                         (sanitize key)))
        (d (isqrt (length key))))
    (make-array (list d d) :displaced-to key-backer)))

(defun hill-product (key block)
  "Dot-product KEY and BLOCK to produce a new vector containing ciphertext."
  (let ((d (length block)))
    (if (= (array-dimension key 0)
           (array-dimension key 1)
           d)
        (loop
          with res = (make-array (list d) :element-type 'fixnum)
          for r below d
          for row = (make-array (list d)
                                :displaced-to key
                                :displaced-index-offset (* r d))
          do (setf (elt res r)
                   (+ (mod (loop for i below d
                                 summing (* (elt row i)
                                            (elt block i)))
                           26)))
          finally (return res))
        (error "Key is not square with block."))))

(defun hill-cipher (key message)
  (let* ((d (array-dimension key 0))
         (message-translated
           (make-array (list
                        (+
                         (length message)
                         (rem (length message) d)))
                       :element-type 'fixnum
                       :initial-element (mini-char-code #\x ))))
    (map-into message-translated #'mini-char-code message)
    (map 'string #'mini-code-char (apply #'concatenate 'vector
                                         (loop for b in (batches message-translated d)
                                               collect (hill-product key b))))))

;; TODO finish implementing this so that you can easily decrypt the hill cipher.
(defun inverse (matrix)
  "Invert MATRIX."
  (let* ((d (array-dimension matrix 0))
         (augmented (make-array (list d
                                      (* 2 d)))))
    (loop for i below (* d d)
          for (x y) = (array-index-row-major matrix i)
          do (setf (aref augmented x y) (aref matrix x y)))
    (loop for i = d then (+ i d d 1)
          while (< i (* d 2 d))
          for (x y) = (array-index-row-major augmented i)
          do (setf (aref augmented x y) 1))


    augmented
    ))

(defun vigenere (key message &optional (enc t))
  "Encipher MESSAGE using KEY according to the Vigenere scheme."
  (let* ((message (sanitize message))
         (key (sanitize key))
         (message-length (length message))
         (key-length (length key))
         (res (make-array (length message) :element-type 'character)))
    (loop for c across (sanitize message)
          for i below message-length
          for j = 0 then (mod i key-length)
          with k = (sanitize key)
          do (setf (elt res i)
                   (mini-code-char (mod (+ (mini-char-code (elt k j))
                                           (mini-char-code (elt message i)))
                                        26)))
          )
    res))

(defmacro permuter (&rest positions)
  `(lambda (input)
     (let ((output (make-array  ,(length (flatten positions )) :element-type 'bit)))
       ,@(remove nil (loop for p in positions
                           for i from 0 upto (length positions)
                           collecting
                           (cond
                             ((null p) nil)
                             ((listp p)
                              `(setf ,@(loop for pos in p
                                             collecting `(elt output ,pos)
                                             collecting `(elt input ,i))))
                             (t `(setf (elt output ,p)
                                       (elt input ,i))))))
       output)))

(fbind* ((permute-initial (permuter 3 5 1 6 7 0 2 4))
         (permute-expand (permuter nil nil nil nil 4 (3 1) 0 (2 5)))
         (permute-contract (permuter 4 5 nil 2 1 3 0))
         (permute-4x4 (permuter nil nil nil nil 1 3 0 2))
         (permute-final (permuter 5 2 6 0 7 1 3 4))
         (sbox
          (let ((subs (make-array '(4 16)
                                  :initial-contents
                                  '((14  4 13  1  2 15 11  8  3 10  6 12  5  9  0  7)  ; Row 0
                                    ( 0 15  7  4 14  2 13  1 10  6 12 11  9  5  3  8)  ; Row 1
                                    ( 4  1 14  8 13  6  2 11 15 12  9  7  3 10  5  0)  ; Row 2
                                    (15 12  8  2  4  9  1  7  5 11  3 14 10  0  6 13)) ; Row 3
                                  )))
            (lambda (s)
              (permute-4x4
               (bit-smasher:bits<-
                (aref subs
                      (bit-smasher:bits->int (list (elt s 0)
                                                   (elt s 5)))
                      (bit-smasher:bits->int (make-array 4
                                                         :element-type 'bit
                                                         :displaced-to s
                                                         :displaced-index-offset 1)))))))))
  
  (defun des (message key rounds
              &aux
                (k (make-array 7 :element-type 'bit
                                 :displaced-to (bit-smasher:bits<- key)))
                (m (permute-initial (bit-smasher:bits<- message)) ))
    (flet ((split-shift (round
                         &aux
                           (r (- (mod (1- (* round 2)) 4))))
             (rotate (make-array 3 :element-type 'bit :displaced-to k) r)
             (rotate (make-array 4 :element-type 'bit :displaced-to k :displaced-index-offset 3) r)
             k))
      (dotimes (round rounds)
        (let* ((rp (permute-expand m))
               (kp (permute-contract (split-shift (1+ round))))
               (s (make-array 6 :element-type 'bit))
               (r1 (make-array 4 :element-type 'bit)))
          (map-into s #'logxor kp rp)
          (map-into r1 #'logxor (sbox s) m)
          (replace m m
                   :start2 4)
          (replace m r1
                   :start1 4)))
      (bit-smasher:bits->int (permute-final m)))))
