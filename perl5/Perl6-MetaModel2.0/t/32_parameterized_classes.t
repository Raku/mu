#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 25;
use Test::Exception;

use Carp 'confess';

use Perl6::MetaModel;

=pod
 

=cut

my $Scalar = class 'Scalar' => [ '::T' ] => sub {
    my %p = @_;
    
    # fetch my generic parameter
    my $T = $p{'::T'};
    
    $::CLASS->superclasses([ $::Object ]);    
    $::CLASS->name('Scalar[' . $T->name . ']');
        
    $::CLASS->add_attribute('$:value' => ::make_attribute('$:value'));    
    
    $::CLASS->add_method('FETCH' => ::make_method(sub { 
        ::opaque_instance_attr($::SELF => '$:value') 
    }));
    
    $::CLASS->add_method('STORE' => ::make_method(sub {
        my ($self, $value) = @_;
        ($value->isa($T->name)) # || $value->does($T)) 
            || confess "Incorrect Type";
        ::opaque_instance_attr($self => '$:value') = $value;
    }));
};

my $Int = class 'Int' => { is => [ $::Object ] };
isa_ok($Int, 'Int');
isa_ok($Int, 'Class');

my $Str = class 'Str' => { is => [ $::Object ] };
isa_ok($Str, 'Str');
isa_ok($Str, 'Class');

my $IntScalar = $Scalar->('::T' => $Int);
isa_ok($IntScalar, 'Scalar[Int]');
isa_ok($IntScalar, 'Class');

my $int_scalar = $IntScalar->new();
isa_ok($int_scalar, 'Scalar[Int]');
isa_ok($IntScalar, 'Class');

my $Five = $Int->new();
isa_ok($Five, 'Int');

my $HelloWorld = $Str->new();
isa_ok($HelloWorld, 'Str');

lives_ok {
    $int_scalar->STORE($Five)
} '... this lives okay';

is($int_scalar->FETCH(), $Five, '... got the right value back');

dies_ok {
    $int_scalar->STORE($HelloWorld);
} '... this dies okay';

my $StrScalar = $Scalar->('::T' => $Str);
isa_ok($StrScalar, 'Scalar[Str]');

my $str_scalar = $StrScalar->new();
isa_ok($str_scalar, 'Scalar[Str]');

lives_ok {
    $str_scalar->STORE($HelloWorld)
} '... this lives okay';

is($str_scalar->FETCH(), $HelloWorld, '... got the right value back');

dies_ok {
    $str_scalar->STORE($Five);
} '... this dies okay';

# make sure the Int still works

lives_ok {
    $int_scalar->STORE($Five)
} '... this lives okay';

is($int_scalar->FETCH(), $Five, '... got the right value back');

dies_ok {
    $int_scalar->STORE($HelloWorld);
} '... this dies okay';

my $int_scalar2 = $IntScalar->new();
isa_ok($int_scalar2, 'Scalar[Int]');

lives_ok {
    $int_scalar2->STORE($Five)
} '... this lives okay';

is($int_scalar2->FETCH(), $Five, '... got the right value back');

dies_ok {
    $int_scalar2->STORE($HelloWorld);
} '... this dies okay';