(in-package :cl-user)
(defpackage lisp-cipher/test
  (:use :cl :fiveam :lisp-cipher :serapeum))
(in-package :lisp-cipher/test)

(def-suite lisp-cipher
  :description "Test the lisp-cipher system.")


(def-suite* fast-modular-exponentiation
  :in lisp-cipher)


(test simple-expt-mod
  (let ((result (expt-mod 4 13 497)))
    (is (= 445 result))))


(def-suite* simple-ciphers
  :in lisp-cipher)

(test caesar
  (let ((result (caesar-cipher "meet me after the toga party")))
    (is (string= "phhw ph diwhu wkh wrjd sduwb" result))))


(test playfair
  (serapeum:fbind ((playfair-cipher (make-playfair "monarchy")))
    (let ((result (playfair-cipher "instruments")))
      (is (string= result "gatlmzclrqxa")))))

(test des
  (is (des #x57 #xAC 2) #xD1))
