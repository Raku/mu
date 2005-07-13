#!/usr/bin/pugs

use v6;
use Test;

plan(11);

unless eval 'eval("1", :lang<perl5>)' {
    skip_rest;
    exit;
}

die unless
eval(q/
package My::Array;
use strict;
print ''; # XXX - voodoo!

sub new {
    my ($class, $ref) = @_;
    bless \$ref, $class;
}

sub array {
    my $self = shift;
    return $$self;
}

sub my_elems {
    my $self = shift;
    return scalar(@{$$self});
}

sub my_exists {
    my ($self, $idx) = @_;
    return exists $$self->[$idx];
}

sub fetch {
    my ($self, $idx) = @_;
    return $$self->[$idx];
}

sub store {
    my ($self, $idx, $val) = @_;
    $$self->[$idx] = $val;
}

sub push {
    my ($self, $val) = @_;
    push @{$$self}, $val;
}

1;
/, :lang<perl5>);

my $p5ar = eval("My::Array", :lang<perl5>);
my @array = (5,6,7,8);
my $p5array = $p5ar.new(\@array);

my $retarray = $p5array.array;

is($p5array.my_elems, @array.elems, 'elems');
is($retarray.elems, @array.elems, 'retro elems', :todo<feature>);

is($p5array.my_exists(1), @array.exists(1), 'exists');
is($retarray.exists(1), @array.exists(1), 'retro exists', :todo<feature>);

is($p5array.my_exists(10), @array.exists(10), 'nonexists fail');
is($retarray.exists(10), @array.exists(10), 'retro nonexists' );

is($p5array.fetch(3), @array[3], 'fetch');

# this access ruins pugs::env below
#lives_ok {
#    is($retarray.[3], @array[3], 'retro fetch');
#}

# XXX - Infinite loop
skip_rest; exit;

$p5array.push(9);

is($p5array.fetch(4), 9, 'push result', :todo<feature>);
is(@array[4], 9, 'push result', :todo<feature>);

#$retarray.push(9);  # this will loop

#is($p5array.fetch(5), 9, 'retro push result');
#is(@array[5], 9, 'retro push result');

$p5array.store(0,3);

is(@array[0], 3, 'store result', :todo<feature>);
is($p5array.fetch(0), 3, 'store result', :todo<feature>);

# TODO: pop, shift, unshift, splice, delete
