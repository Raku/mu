#!/usr/bin/pugs

use v6;
use Test;

plan 8;

{
    my $x = Inf;
    
    is( $x, Inf,   'numeric equal' );
    is( $x, 'Inf', 'string equal'  );
}

{
    my $x = -Inf;
    is( $x, -Inf,   'negative numeric equal' );
    is( $x, '-Inf', 'negative string equal'  );
}

{
    my $x = int( Inf );
    is( $x, Inf,   'int numeric equal', :todo<bug> );
    is( $x, 'Inf', 'int string equal', :todo<bug> );
}

{
    my $x = int( -Inf );
    is( $x, -Inf,   'int negative numeric equal', :todo<bug> );
    is( $x, '-Inf', 'int negative string equal', :todo<bug> );
}
