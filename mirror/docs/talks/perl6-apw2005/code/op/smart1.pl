#=Perl 5: Match-Operator
$var =~ /regex/;
#=Perl 6: generalisierter Smart-Match-Operator
$s ~~ 1978;
$s ~~ "Hallo, Welt";
$s ~~ /regex/;
$s !~ funktion();
#=Alles zusammen: (änlich wie any)
$s ~~ (123, funktion, /regex/)
($a, $b, $c) !~ ($d, $e, $f)
