(asdf:defsystem :lisp-crypto
  :depends-on (:alexandria :serapeum)
  :components ((:file "package")
               (:file "crypto"))
  :in-order-to ((test-op (test-op :lisp-crypto/tests))))

(asdf:defsystem :lisp-crypto/tests
  :description "Test suite for the lisp-crypto system"
  :author "Judah Sotomayor <judah.sotomayor@pm.me>"
  :version "0.1.0"
  :depends-on (:lisp-crypto
               :fiveam)
  :serial t
  :components ((:file "package")
               (:file "test"))
  :perform (test-op (op c)
                    (symbol-call :fiveam :run!
                                 (find-symbol* :lisp-crypto :lisp-crypto/test))))
