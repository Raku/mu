
use v6-alpha;

module Main {

say '1..1';

my $str = 'not ok ';
say substr( $str, 4 ) ~ "1";
say substr( $str, 4 ), "2";

}
