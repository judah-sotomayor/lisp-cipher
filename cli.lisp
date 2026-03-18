(in-package :cl-user)
(defpackage lisp-cipher/cli
  (:use :cl :alexandria :serapeum)
  (:import-from :clingon)
  (:export
   :main))
(in-package :lisp-cipher/cli)

(defun prompt-read (prompt)
  (format *query-io* "~a" prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun des/options ()
  "Returns the options for lisp-cipher's DES command."
  (list))

(defun des/handler (cmd)
  (declare (ignore cmd))
  (let ((plaintext (parse-integer (prompt-read "Enter Plaintext (HEX): ") :radix 16))
        (key (parse-integer (prompt-read  "Enter Key (HEX): ") :radix 16))
        (rounds (parse-integer (prompt-read  "Rounds (DEC): ") :radix 10)))
    (format *query-io* "Ciphertext (HEX): ~x~%" (lisp-cipher:des plaintext key rounds))))

(defun des/command ()
  (clingon:make-command :name "DES"
                        :description "Calculate the DES algorithm on a one-byte key and one-byte message."
                        :handler #'des/handler
                        :options (des/options)))

(defun website/options ()
  "Return the options for lisp-cipher's website command."
  (list
   (clingon:make-option
    :string
    :description "IP to bind on"
    :short-name #\b
    :long-name "bind"
    :initial-value "127.0.0.1"
    :key :ip)
   (clingon:make-option
    :integer
    :description "Port to bind on"
    :short-name #\p
    :long-name "port"
    :initial-value 8000
    :key :port)))

(defun website/handler (cmd)
  (let ((ip (clingon:getopt cmd :ip))
        (port (clingon:getopt cmd :port)))
    (let ((server (lisp-cipher/website:start ip port)))
      (handler-case
          (loop (sleep 1))
        (sb-sys:interactive-interrupt ()
          (format t "~%Shutting down...~%")))
      (clack:stop server))))

(defun website/command ()
  (clingon:make-command :name "website"
                        :description "Launch the lisp-cipher website"
                        :handler #'website/handler
                        :options (website/options)))

(defun top-level/options ()
  (list))

(defun top-level/sub-commands ()
  (list (des/command)
        (website/command)))

(defun top-level/handler (cmd)
  (clingon:print-usage-and-exit cmd t))

(defun top-level/command ()
  (clingon:make-command :name "lisp-cipher"
                        :version "0.1.0"
                        :description "A simple lisp-cipher CLI tool"
                        :authors '("Judah Sotomayor <judah.sotomayor@pm.me>")
                        :handler #'top-level/handler
                        :options (top-level/options)
                        :sub-commands (top-level/sub-commands)))

(defun main ()
  "The  main entrypoint of the CLI"
  (let ((app (top-level/command)))
    (clingon:run app)))
