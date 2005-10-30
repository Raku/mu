#!/usr/bin/pugs

use v6;
use Test;

=pod

The parser won't do right thing when two(or more) class-es get
attributes whose name are the same.



=cut

plan 3;


{
    my $var = 100;
    class a1 {
        has $.a;
        has $.c;
        method update { $var -= $.a; }
    };
    a1.new( a => 10 ).update;
    is $var, 90, "Testing suite 1.";
}



{
    my $var = 100;
    class a2 {
        has $.a;
        method update { $var -= $.a; }
    };
    class b2 {
        has $.a;
        submethod BUILD { a2.new( a => $.a ).update; };
    };
    b2.new( a => 20 );
    is $var, 80, "Testing suite 2.";
}



{
    my $var = 100;
    class a3 {
        has $.a;
        method update { $var -= $.a; }
    };
    class b3 {
        has $.a;
        submethod BUILD { a3.new( a => $.a ).update; }
    };
    class c3 {
        has $.b;
        submethod BUILD { b3.new( a => $.b ); }
    };

    c3.new( b => 30 );
    is $var, 70, "Testing suite 3.";
}
