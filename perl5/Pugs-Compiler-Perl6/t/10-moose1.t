use v6-**;

use lib './t', '.';
use moose1;

say '1..2';
say 'ok 1 # use Moose';

my $p = Point.new;
$p.x(10);

$p.x != 10 and print 'not ';
say 'ok 2 # set x'; 
