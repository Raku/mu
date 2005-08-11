#=Zuweisungsoperator
$x = $y; # wie bei Perl 5, erzeugt Kopie

#=Bindungsoperator
$x := $y; # $x ist nun der selbe Container wie $y

#=Bindungsoperator (statisch)
$x ::= $y; # $x ist nun die selbe Variable wie $y
