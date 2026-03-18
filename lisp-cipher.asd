(asdf:defsystem :lisp-cipher
  :depends-on (:alexandria :serapeum :bit-smasher)
  :components ((:file "cipher"))
  :in-order-to ((asdf:test-op (asdf:test-op :lisp-cipher/tests))))

(asdf:defsystem :lisp-cipher/cli
  :description "Command-line interface for the lisp-cipher system"
  :author "Judah Sotomayor <judah.sotomayor@pm.me"
  :version "0.1.0"
  :depends-on (:lisp-cipher
               :clack
               :clack-handler-hunchentoot
               :clingon
               :spinneret
               :ningle)
  :serial t
  :components ((:file "website")
               (:file "cli"))
  :build-operation "program-op"
  :build-pathname "lisp-cipher"
  :entry-point "lisp-cipher/cli:main")

(asdf:defsystem :lisp-cipher/tests
  :description "Test suite for the lisp-cipher system"
  :author "Judah Sotomayor <judah.sotomayor@pm.me>"
  :version "0.1.0"
  :depends-on (:lisp-cipher
               :fiveam)
  :serial t
  :components ((:file "test"))
  :perform (asdf:test-op (op c)
                         (symbol-call :fiveam :run!
                                      (find-symbol* :lisp-cipher :lisp-cipher/test))))

;; "(sb-ext:save-lisp-and-die #p\"lisp-cipher-website\" :toplevel #'lisp-cipher/website:start :executable t)"
