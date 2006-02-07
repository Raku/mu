#!/usr/bin/pugs

use v6;
use Test;

plan 1;

my $var = 5;

role B { method x { $var = 3; } }

class T does B { }

class S does B
{
        has $.t;
        method x
        {
                "always repeated".say;
                $.t.x;
        }
        method BUILD
        { $.t = T.new }
}

# uncomment below after the bug is fixed. As below line will cause infinite loop;
#S.new.x;

is $var, 3, "Test class inhrited from the same role caused infinite loop bug", :todo<bug>;

