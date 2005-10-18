#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 6;
use Test::Exception;

use Perl6::MetaModel;

=pod

Anonymous class tests from t/oo/class/anonymous.t

=cut

my $class;
lives_ok {
    $class = class { 
        'is' => [ $::Object ], 
        'methods' => {
            'meth' => sub { 42 }
        }
    };
} '... anonymous class created';
isa_ok($class, 'Class');
ok($class->is_a($::Object), '... $class is a $::Object');

my $a;
lives_ok {
    $a = $class->new;
} '... instantiation of anonymous class';
is($a->meth, 42, '... calling a method on an instance of an anonymous class (1)');

# And the same w/o using a $class variable:

is(class({'is'=>[$::Object],'methods'=>{'meth'=> sub { 42 }}})->new->meth, 
   42, 
   '... the whole thing can also be done inline')
