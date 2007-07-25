
use v6-alpha;

module Main {

say '1..2';

my $str = 'not ok ';
say substr( $str, 4 ) ~ "1";
say substr( $str, 4 ), "2";

}
