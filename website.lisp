(in-package :cl-user)
(defpackage lisp-cipher/website
  (:use :cl :alexandria :serapeum)
  (:export
   :start))
(in-package :lisp-cipher/website)

(defvar *app* (make-instance 'ningle:app))

(defmacro with-page ((&key title) &body body)
  (alexandria:once-only (title)
    `(spinneret:with-html-string
       (:doctype)
       (:html
        (:head
         (:title ,title)
         (:script :src "https://cdn.jsdelivr.net/npm/htmx.org@2.0.8/dist/htmx.min.js" :integrity "sha384-/TgkGk7p307TH7EXJDuUlgG3Ce1UVolAOFopFekQkkXihi5u/6OCvVKyz1W+idaz" :crossorigin "anonymous"))

        (:body
         (:h1 ,title)
         ,@body)))))

(setf (ningle:route *app* "/")
      (lambda (params)
        (declare (ignore params))
        (with-page (:title "Lisp Ciphers" )
          (:em "Judah Sotomayor")
          (:p "lisp-cipher is a package with simple implementations of a few common ciphers, completed for a class.")

          (:div
           (:h2 "Data Encryption Standard")
           (:form :id "des-input"
                  :hx-post "/des"
                  :hx-target "#des-result"
                  :hx-swap "innerHTML"
                  (:div (:label :for "des-plaintext" "Enter plaintext (HEX): ")
                        (:input :id "des-plaintext" :name "des-plaintext" :type "test" :maxlength 2 :pattern "[0-9a-fA-F]{1,2}" :value "57"))
                  (:div (:label :for "des-key" "Enter key (HEX): " )
                        (:input :id "des-key" :name "des-key" :type "text" :maxlength 2 :pattern "[0-9a-fA-F]{1,2}" :value "AC"))
                  (:div (:label :for "des-rounds" "Rounds (DEC): ")
                        (:input :id "des-rounds" :name "des-rounds" :type "number" :min 1 :max 16 :value 2))
                  (:button "Submit"))
           (:h3 "result")
           (:div :id "des-result")))))

(setf (ningle:route *app* "/des" :method :POST)
      (lambda (params)
        (handler-case (let ((plaintext (parse-integer (assocdr "des-plaintext" params :test #'string=) :radix 16))
                            (key (parse-integer (assocdr "des-key" params :test #'string=) :radix 16))
                            (rounds (parse-integer (assocdr "des-rounds" params :test #'string=))))
                        (fmt "~x" (lisp-cipher:des plaintext key rounds)))
          (t ()
            "Invalid input!"))))


(defmethod ningle:not-found ((app ningle:<app>))
  (declare (ignore app))
  (setf (lack.response:response-status ningle:*response*) 404)
  "Not Found")

(defun start (&key (server :hunchentoot) (address "127.0.0.1") (port 8000))
  (clack:clackup
   *app*
   :server server
   :address address
   :port port
   ))
