cat liblisp/MiniPerl6/Lisp/Runtime.lisp \
    liblisp/MiniPerl6/Lisp/Prelude.lisp \
  > tmp.lisp
perl -Ilib5 mp6-lisp.pl < $1 >> tmp.lisp

sbcl --script tmp.lisp
