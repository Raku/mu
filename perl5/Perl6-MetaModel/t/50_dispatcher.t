#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 9;
use Data::Dumper;

use Perl6::MetaModel;
    
class Foo => {};   
class Bar => {
    is => [ 'Foo' ]
};
class Baz => {};
class FooBar => {
    is => [ 'Foo', 'Bar' ]
};
class FooBarBaz => {
    is => [ 'FooBar', 'Baz' ]
};
    


my $i = FooBarBaz->meta->dispatcher();

my @control = qw(
    FooBarBaz
        FooBar
            Foo
                Perl6::Object
            Bar
                Foo
                    Perl6::Object
        Baz
            Perl6::Object    
);

my $metaclass = $i->();
while (defined $metaclass) {
    is($metaclass->name, shift(@control), '... got the metaclass we expected');
    $metaclass = $i->();  
}

1;

__END__
