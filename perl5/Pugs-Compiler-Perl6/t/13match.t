
use v6-pugs;

say "1..1";

my $str = 'abcbcaddd';

$str ~~ s:P5:g/bc/oz/;

if $str eq 'aozozaddd' { say "ok 1" } else { say "not ok 1" }

1;
