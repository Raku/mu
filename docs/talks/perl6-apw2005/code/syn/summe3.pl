#=summe3.pl - Unicode in Perl 6
use v6;
sub Σ { [+] @_ }

say Σ 1..10; # gibt 55 aus
