# Compile MiniPerl6 to lisp, using mp6-perl5-boot.pl

#rm -rf liblisp
mkdir liblisp
mkdir liblisp/MiniPerl6
mkdir liblisp/MiniPerl6/Grammar
mkdir liblisp/MiniPerl6/Lisp
mkdir liblisp/MiniPerl6/Emitter
mkdir liblisp/MiniPerl6/Perl5
mkdir liblisp/MiniPerl6/Perl5MO
mkdir liblisp/MiniPerl6/Parrot
mkdir liblisp/MiniPerl6/PAST
mkdir liblisp/MiniPerl6/Perl6Parrot

cp lib/MiniPerl6/Lisp/Runtime.lisp liblisp/MiniPerl6/Lisp/

perl mp6-lisp.pl \
    <      lib/MiniPerl6/Lisp/Prelude.pm    \
    >  liblisp/MiniPerl6/Lisp/Prelude.lisp

perl mp6-lisp.pl \
    <      lib/Test.pm    \
    >  liblisp/Test.lisp

perl mp6-lisp.pl \
    <      lib/MiniPerl6/Lisp/Emitter.pm    \
    >  liblisp/MiniPerl6/Lisp/Emitter.lisp

