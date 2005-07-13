#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 10;
use Data::Dumper;

use Perl6::MetaModel;

class 'Dog-0.0.1-cpan:JRANDOM' => {
    class => {
        methods => {
            growl => sub { 'GRRRRR' }
        }
    },
    instance => {
        methods => {
            bark => sub { 'WOOF' }
        }
    }
};

is(Dog->growl(), 'GRRRRR', '... the Dog class growled correctly');

my $dog1 = Dog->new();
isa_ok($dog1, 'Dog');

is($dog1->bark(), 'WOOF', '... the Dog barked correctly');

# and now the full name too

is('Dog-0.0.1-cpan:JRANDOM'->growl(), 'GRRRRR', '... the Dog-0.0.1-cpan:JRANDOM class growled correctly');

my $dog2 = 'Dog-0.0.1-cpan:JRANDOM'->new();
isa_ok($dog2, 'Dog');

is($dog2->bark(), 'WOOF', '... the Dog-0.0.1-cpan:JRANDOM barked correctly');

class Fido => {
    is => [ 'Dog-0.0.1-cpan:JRANDOM' ]
};

is(Fido->growl(), 'GRRRRR', '... the Fido class growled correctly');

my $fido = Fido->new();
isa_ok($fido, 'Fido');
isa_ok($fido, 'Dog');

is($fido->bark(), 'WOOF', '... the Fido barked correctly');

