use v6-pugs;

use lib './t', '.';
use moose1;

say '1..1';
say '1 ok # use Moose';

=begin
use v6-pugs;
class Point;

has $.x is rw;  # instance attributes
has $.y;        # default "is readonly" 
method clear () {
    $.x = 0;  # accessible within the class
    $.y = 0;
}
=end
