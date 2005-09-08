# ads
# aasd
$a = "a#" ~ foo("test"); # test
sub infix:«>?» ($a, $b) { $a > $b ?? $!!:: $b }
sub postfix:<!> ($x) { [*] 1..$x >? 1 };
# sechs mal in Perl 5 würfeln
@a = 0; push @a, wurf() for 1..6;
sub ε {}
