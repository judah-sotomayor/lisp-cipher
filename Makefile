cli:
	qlot exec sbcl --load lisp-cipher.asd \
	     --eval '(asdf:load-system :lisp-cipher/cli)' \
         --eval '(asdf:make :lisp-cipher/cli)' \
         --eval '(quit)'

