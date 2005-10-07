#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 22;
use Test::Exception;

use Carp 'confess';

use Perl6::MetaModel;

=pod

This test checks that a parameterized class like Array can 
handle nested parameterized instnaces of itself. Such as

- Array of Array of Int

=cut

my $Array = class 'Array' => [ '::T' ] => sub {
    my %p = @_;
    # fetch my generic parameter
    my $T = $p{'::T'};

    $::CLASS->superclasses([ $::Object ]);    
    $::CLASS->name('Array[' . $T->name . ']');

    $::CLASS->add_attribute('@:values' => ::make_attribute('@:values'));    

    $::CLASS->add_method('STORE' => ::make_method(sub {
        my ($self, $index, $value) = @_;
        ($value->isa($T->name) || $value->does($T)) 
            || confess "Incorrect Type";
        ::opaque_instance_attr($self => '@:values')->[$index] = $value;
    }));

    $::CLASS->add_method('FETCH' => ::make_method(sub { 
        my ($self, $index) = @_;
        ::opaque_instance_attr($::SELF => '@:values')->[$index];
    }));    
};

my $Int = class 'Int' => { 
    is => [ $::Object ],
    attributes => [ '$:num' ],
    methods => {
        'num' => sub { _('$:num') }
    }
};
isa_ok($Int, 'Int');

my $Str = class 'Str' => { is => [ $::Object ] };
isa_ok($Str, 'Str');

my $ArrayOfInt = $Array->('::T' => $Int);
isa_ok($ArrayOfInt, 'Array[Int]');

my $ArrayOfArrayOfInt = $Array->('::T' => $ArrayOfInt);
isa_ok($ArrayOfArrayOfInt, 'Array[Array[Int]]');

# now do something, and see what happens

my $array_of_int = $ArrayOfInt->new();
isa_ok($array_of_int, 'Array[Int]');

lives_ok {
    $array_of_int->STORE(0, $Int->new('$:num' => 1));
    $array_of_int->STORE(1, $Int->new('$:num' => 2));
    $array_of_int->STORE(2, $Int->new('$:num' => 3));
} '... STORE-ing values in Array[Int] worked';

isa_ok($array_of_int->FETCH(0), 'Int');
isa_ok($array_of_int->FETCH(1), 'Int');
isa_ok($array_of_int->FETCH(2), 'Int');

is($array_of_int->FETCH(0)->num, 1, '... got the number we expected');
is($array_of_int->FETCH(1)->num, 2, '... got the number we expected');
is($array_of_int->FETCH(2)->num, 3, '... got the number we expected');

dies_ok {
    $array_of_int->STORE(3, $Str->new());
} '... STORE-ing bad values in Array[Int] failed (as expected)';

my $array_of_array_of_int = $ArrayOfArrayOfInt->new();
isa_ok($array_of_array_of_int, 'Array[Array[Int]]');

dies_ok {
    $array_of_array_of_int->STORE(0, $Int->new('$:num' => 1));
} '... STORE-ing bad values in Array[Array[Int]] failed (as expected)';

dies_ok {
    $array_of_array_of_int->STORE(0, $Str->new());
} '... STORE-ing bad values in Array[Array[Int]] failed (as expected)';

lives_ok {
    $array_of_array_of_int->STORE(0, $array_of_int);
} '... STORE-ing the correct value-type in Array[Array[Int]] worked';

is($array_of_array_of_int->FETCH(0)->FETCH(0)->num, 1, '... got the number we expected');
is($array_of_array_of_int->FETCH(0)->FETCH(1)->num, 2, '... got the number we expected');
is($array_of_array_of_int->FETCH(0)->FETCH(2)->num, 3, '... got the number we expected');

lives_ok {
    $array_of_array_of_int->FETCH(0)->STORE(3, $Int->new('$:num' => 4));
} '... STORE-ing values in Array[Int] through Array[Array[Int]] worked';

is($array_of_array_of_int->FETCH(0)->FETCH(3)->num, 4, '... got the number we expected');
