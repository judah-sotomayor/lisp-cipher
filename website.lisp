(in-package :cl-user)
(defpackage lisp-cipher/website
  (:use :cl :alexandria :serapeum)
  (:export
   :start))
(in-package :lisp-cipher/website)
(eval-when (:compile-toplevel :execute :load-toplevel)
  (pushnew "_" spinneret:*unvalidated-attribute-prefixes* :test #'equal))

(defvar *app* (make-instance 'ningle:app))

(defmacro with-page ((&key title) &body body)
  (alexandria:once-only (title)
    `(spinneret:with-html-string
       (:doctype)
       (:html
        (:head
         (:title ,title)
         (:script :src "https://cdn.jsdelivr.net/npm/htmx.org@2.0.8/dist/htmx.min.js" :integrity "sha384-/TgkGk7p307TH7EXJDuUlgG3Ce1UVolAOFopFekQkkXihi5u/6OCvVKyz1W+idaz" :crossorigin "anonymous"))
        (:script :src "https://unpkg.com/hyperscript.org@0.9.14" :integrity "sha384-NzchC8z9HmP/Ed8cheGl9XuSrFSkDNHPiDl+ujbHE0F0I7tWC4rUnwPXP+7IvVZv" :crossorigin "anonymous")

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
           (:h3 :id "des-result" "Result: ")
           )
          (:hr)
          (:div
           (:h2 "Playfair Cipher")
           (:form :id "playfair-input"
                  :hx-target "#playfair-result"
                  :hx-swap "outerHTML"
                  (:div (:label :for "playfair-key" "Key" )
                        (:input :id "playfair-key" :name "playfair-key" :type "text" :maxlength 25))
                  (:div (:label :for "playfair-message" "Message")
                        (:textarea :id "playfair-message" :name "playfair-message"))
                  (:button :name "encrypt" :hx-post "/playfair/encrypt" 
                           "Encrypt")
                  (:button :name "decrypt" :hx-post "/playfair/decrypt" 
                           "Decrypt"))
           (:div (:h3 "Result")
                 (:textarea :readonly t :id "playfair-result"))

           (:button :_
                    "on click set content to #playfair-message's value then
set #playfair-message's value to #playfair-result's value then
set #playfair-result's value to content" "Swap result and message")))))

(setf (ningle:route *app* "/playfair/:action" :method :POST)
      (lambda (params)
        (let ((operation (switch ((assocdr :action params) :test #'equal)
                           ("encrypt" :encrypt)
                           ("decrypt" :decrypt)
                           (t (error "Bad post!"))))


              (message (assocdr "playfair-message" params :test #'string=))
              (key (assocdr "playfair-key" params :test #'string=)))
          (fbind ((pf (lisp-cipher:make-playfair key)))
            (spinneret:with-html-string (:textarea :readonly t :id "playfair-result" (if (eq :encrypt operation)
                                                                                         (pf message t)
                                                                                         (pf message nil))))))))

(setf (ningle:route *app* "/des" :method :POST)
      (lambda (params)
        (handler-case (let ((plaintext (parse-integer (assocdr "des-plaintext" params :test #'string=) :radix 16))
                            (key (parse-integer (assocdr "des-key" params :test #'string=) :radix 16))
                            (rounds (parse-integer (assocdr "des-rounds" params :test #'string=))))
                        (fmt "Result: ~x" (lisp-cipher:des plaintext key rounds)))
          (t ()
            "Invalid input!"))))


(defmethod ningle:not-found ((app ningle:<app>))
  (declare (ignore app))
  (setf (lack.response:response-status ningle:*response*) 404)
  "Not Found")

(defun start (&optional (address "127.0.0.1") (port 8000))
  (clack:clackup
   *app*
   :server :hunchentoot
   :address address
   :port port
   ))
