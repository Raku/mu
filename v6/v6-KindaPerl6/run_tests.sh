#!/bin/sh
for f in `ls t/p5/*.t` ; do echo $f ; perl -Ilib5 $f; done
for f in `ls t/insanity/*.t` ; do echo $f ; perl kp6-perl5.pl < $f | perl -Ilib5 ; done
for f in `ls t/kp6/*.t` ; do echo $f ; perl kp6-perl5.pl < $f | perl -Ilib5 ; done
