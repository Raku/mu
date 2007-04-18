for f in `ls t/kp6/*.t` ; do echo $f ; perl kp6-perl5.pl < $f | perl -Ilib5 ; done
