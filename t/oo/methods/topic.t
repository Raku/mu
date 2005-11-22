#!/usr/bin/pugs

use v6;
use Test;

# L<A12/"Declaration of Methods" /methods do not set the topic now/>
# (This is an an "update" section.)

plan 2;

class Foo {
    method no_topic     { $_.echo }
    method topic ($_: ) { $_.echo }
    method echo         { "echo"  }
}

{
    my Foo $foo .= new;
    dies_ok { $foo.no_topic() }, '$_ is not set in methods...';
}

{
    my Foo $foo .= new;
    is $foo.topic(), "echo", '...unless $_ the invocant name is specified to be "$_"';
}
