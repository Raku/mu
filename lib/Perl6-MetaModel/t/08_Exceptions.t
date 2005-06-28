#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;

use MetaModel;
use Data::Dumper;

=pod

=cut

class Exception => {
    class => {
        attrs => [ '$.call_stack' ]
    },
    kind => {
        methods => {
            throw => sub {
                my $class = shift;
                my @call_stack;
                my $i = 0;
                my @c;
                while (@c = CALLER($i++)) {
                    push @call_stack => [ @c ];
                }
                #warn Data::Dumper::Dumper \@call_stack;
                die $class->new_instance('$.call_stack' => \@call_stack);
            }
        }
    }  
};

class Foo => {
    class => {
        methods => {
            foo => sub { (shift)->bar()     },
            bar => sub { (shift)->baz()     },
            baz => sub { Exception->throw() }
        }
    }  
};

my $foo = Foo->new_instance();
isa_ok($foo, 'Foo');

$@ = undef;
eval {
    $foo->foo();
};
ok($@, '... got an exception');

is_deeply(
    $@->call_stack(),
    [
        [ 'Exception', 'throw' ],   
        [ 'Foo', 'baz' ],
        [ 'Foo', 'bar' ],
        [ 'Foo', 'foo' ],
    ],
    '... our exception stack_trace() is what we expect');




