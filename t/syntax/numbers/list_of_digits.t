use v6;

use Test;

plan 4;

=begin pod

Tests for the :x[ <list> ] notations

=end pod

# L<S02/Literals/"Alternately you can use a list of digits in decimal">

is( :60[12,34,56],     12 * 3600 + 34 * 60 + 56, 'List of numbers works' );
is( :100[3,'.',14,16],     3.1416,         'Decimal point in list works' );

# previously in misc.t
is :100[10,10],      1010, "Adverbial form of base 100 integer works";
is :100[10,'.',10], 10.10, "Adverbial form of base 100 fraction works";
