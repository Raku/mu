Kommentare werden weiterhin mit # eingeleitet:

#!\begin{block}{}\texttt{
# dies ist ein Kommentar :-)
#!}\end{block}

Zum auskommentieren mehrerer Zeilen kann man POD benutzen:

#!\begin{block}{}\texttt{
=begin COMMENT
 blah; # ist das so richtig?
 =end COMMENT
#!}\end{block}

Neu in Perl 6 ist, das vor und hinter den Zeilen mit dem
Gleichheitszeichen keine leeren Zeilen mehr sein müssen.
