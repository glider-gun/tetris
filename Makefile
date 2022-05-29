LISP ?= sbcl

ql:	
	$(LISP)	--eval "(ql:register-local-projects)" \
 								--eval "(asdf:load-system :tetris)" \
               				 --eval "(tetris:main)"
run:
	$(LISP) --eval "(asdf:load-system :tetris)" \
                --eval "(tetris:main)"

create:
	$(LISP)	--eval "(ql:register-local-projects)" \
 								--eval "(asdf:load-system :tetris)" \
               				 --eval "(tetris:make-tetris-command)"

swank:
	$(LISP) --eval "(ql:quickload '(:swank) :silent t)" \
                --eval "(swank:create-server :port 5555 :dont-close t)" \
                --eval "(loop (sleep 1.0))"
