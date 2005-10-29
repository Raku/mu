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
    class a {
        has $.a;
        has $.c;
        method update { $var -= $.a; }
    };
    eval 'a.new( a => 10 ).udpate';
    is $var, 90, "Testing suite 1.";
}



{
    my $var = 100;
    class a {
        has $.a;
        method udpate { $var -= $.a; }
    };
    class b {
        has $.a;
        method BUILD { a.new( a => $.a ).udpate; };
    };
    eval 'b.new( a => 20 ).udpate';
    is $var, 80, "Testing suite 2.";
}



{
##### the example below will cause pugs hang.
    my $var = 100;
    class a {
        has $.a;
        method udpate { $var -= $.a; }
    };
    class b {
        has $.a;
        method BUILD { a.new( a => $.a ); }
    };
    class c {
        has $.b;
        method BUILD { b.new( a => $.b ); }
    };

    eval 'c.new( b => 30 ).udpate';
    is $var, 70, "Testing suite 3.";
}
