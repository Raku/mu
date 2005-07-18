#!/usr/bin/pugs

use v6;
use Test;

plan 29;

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
    is( $x, Inf,   'int numeric equal' );
    is( $x, 'Inf', 'int string equal'  );
}

{
    my $x = int( -Inf );
    is( $x, -Inf,   'int negative numeric equal' );
    is( $x, '-Inf', 'int negative string equal'  );
}
