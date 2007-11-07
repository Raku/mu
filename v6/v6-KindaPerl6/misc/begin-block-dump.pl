# - kp6 needs a way to export subs to the runtime

use Data::Dump::Streamer;
use strict;

sub X { print "ok" }

print Dump( \&X );

# this is the algorithm for keeping the compile-time environment in pure-perl

# incrementally set environment; and keep a pad stack

use strict;
my @v;
$v[0] = do { my $x = 3; sub { $x; eval $_[0] } };  # set up closure

$v[0]( ' print "x=$x\n" ' );   # execute in this context level
print "sub=$v[0]\n";

$v[1] = $v[0]( ' do { my $y = 4; sub { $y; eval $_[0] } } ' );  # add a pad level

$v[1]( ' print "y=$y\n" ' );   # execute in this context level

$v[2] = $v[1]( ' do { my $z = 7; sub { $z; eval $_[0] } } ' );  # add a pad level

$v[2]( ' $y++ ' );   # execute in this context level
$v[2]( ' print "y=$y\n" ' );   # execute in this context level
$v[2]( ' print "done\n" ' );   # execute in this context level
$v[2]( ' my $k = $y + 1; print "k=", $k, "\n" ' );   # execute in this context level

$v[3] = $v[2]( ' do { my $x = 9; sub { $x; eval $_[0] } } ' );  # add a pad level

print "Dump:\n", Dump( \@v );

__END__
