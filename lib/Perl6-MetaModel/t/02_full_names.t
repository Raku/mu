#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 39;
use Data::Dumper;

use Perl6::MetaModel;

class 'Dog-0.0.1-cpan:JRANDOM' => {
    instance => {
        methods => {
            bark => sub { 'WOOF' }
        }
    }
};

my $dog1 = Dog->new();
isa_ok($dog1, 'Dog');

is($dog1->bark(), 'WOOF', '... the Dog barked correctly');

my $dog2 = 'Dog-0.0.1-cpan:JRANDOM'->new();
isa_ok($dog2, 'Dog');

is($dog2->bark(), 'WOOF', '... the Dog-0.0.1-cpan:JRANDOM barked correctly');

class Fido => {
    is => [ 'Dog-0.0.1-cpan:JRANDOM' ]
};

my $fido = Fido->new();
isa_ok($fido, 'Fido');
isa_ok($fido, 'Dog');

is($fido->bark(), 'WOOF', '... the Fido barked correctly');

