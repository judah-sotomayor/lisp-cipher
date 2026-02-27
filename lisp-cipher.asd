(asdf:defsystem :lisp-cipher
  :depends-on (:alexandria :serapeum)
  :components ((:file "package")
               (:file "cipher"))
  :in-order-to ((test-op (test-op :lisp-cipher/tests))))

(asdf:defsystem :lisp-cipher/tests
  :description "Test suite for the lisp-cipher system"
  :author "Judah Sotomayor <judah.sotomayor@pm.me>"
  :version "0.1.0"
  :depends-on (:lisp-cipher
               :fiveam)
  :serial t
  :components ((:file "package")
               (:file "test"))
  :perform (test-op (op c)
                    (symbol-call :fiveam :run!
                                 (find-symbol* :lisp-cipher :lisp-cipher/test))))
