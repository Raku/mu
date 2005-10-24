#!/usr/bin/pugs

use v6;
use Test;

plan 8;

{
    my $x = Inf;
    
    cmp_ok( $x, &infix:<==>, Inf,   'numeric equal' );
    cmp_ok( $x, &infix:<eq>, 'Inf', 'string equal'  );
}

{
    my $x = -Inf;
    cmp_ok( $x, &infix:<==>, -Inf,   'negative numeric equal' );
    cmp_ok( $x, &infix:<eq>, '-Inf', 'negative string equal'  );
}

{
    my $x = int( Inf );
    cmp_ok( $x, &infix:<==>,  Inf,  'int numeric equal' );
    cmp_ok( $x, &infix:<eq>, 'Inf', 'int string equal', :todo<bug> );
}

{
    my $x = int( -Inf );
    cmp_ok( $x, &infix:<==>,  -Inf,   'int negative numeric equal');
    cmp_ok( $x, &infix:<eq>, '-Inf',  'int negative string equal', :todo<bug> );
}

# Inf should == Inf. Additionally, Inf's stringification (~Inf), "Inf", should
# eq to the stringification of other Infs.
# Thus:
#     Inf == Inf     # true
# and:
#     Inf  eq  Inf   # same as
#     ~Inf eq ~Inf   # true
