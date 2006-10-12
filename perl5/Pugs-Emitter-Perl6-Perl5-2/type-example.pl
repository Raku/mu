package Typed;
    use overload (
        'x='  => sub { 
            #print "overloaded x= ${$_[0]} $_[1]\n";
            # <[particle]> yep, check if it can ->int, and call it if so, otherwise die or something
            warn "not a number: $_[1]" if $_[1] ne $_[1]+0;
            ${$_[0]} = $_[1];
            $_[0];
        },
        '='  => sub { 
            #print "overloaded = ${$_[0]} $_[1]\n";
            #warn "not a number: $_[1]" if $_[1] ne $_[1]+0;
            ${$_[0]} = $_[1];
            $_[0];
        },
        #'${}'  => sub { 
        #    $_[0][0] 
        #},
        fallback => 0,
    );

package main;
use Data::Dump::Streamer;

$x = bless \( do{ my $v } ), 'Typed';

$y = $x; 
$x x= 3; 
#print "x is a ",ref($x),"\n", Dump($x);
#print "y is a ",ref($y),"\n", Dump($y);

print $$y, " typed y (1)\n"; 
$y x= 4; 
print $$x, "\n";

$y x= 42;
print $$y, " typed y (2)\n"; 
print $$x, "\n";
$y x= 'a';

$x x= 42;
print $$y, " typed x (3)\n"; 
print $$x, "\n";
$x x= 'a';

