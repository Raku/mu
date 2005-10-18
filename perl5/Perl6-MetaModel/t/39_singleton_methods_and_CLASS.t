#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 11;

use Perl6::MetaModel;

## test how it deals with $?CLASS and $?PACKAGE

my $Test = class 'Test' => {
    is => [ $::Object ],
    methods => {
        'instance_test_CLASS'   => sub { $::CLASS   },
        'instance_test_PACKAGE' => sub { $::PACKAGE },        
    }
};
isa_ok($Test, 'Test');

my $test = $Test->new();

is($test->instance_test_CLASS,   $Test, '... got the right value back from the instance method for $?CLASS');
is($test->instance_test_PACKAGE, $Test, '... got the right value back from the instance method for $?PACKAGE');

$Test->add_singleton_method('test_CLASS'   => ::make_method(sub { $::CLASS   }));
$Test->add_singleton_method('test_PACKAGE' => ::make_method(sub { $::PACKAGE }));

is($test->instance_test_CLASS,   $Test, '... got the right value back from the instance method for $?CLASS (after singleton methods)');
is($test->instance_test_PACKAGE, $Test, '... got the right value back from the instance method for $?PACKAGE (after singleton methods)');

is($Test->test_CLASS,   $Test, '... got the right value back from the class method for $?CLASS');
is($Test->test_PACKAGE, $Test, '... got the right value back from the class method for $?PACKAGE');

# and with inheritence

my $Test2 = class 'Test2' => {
    is => [ $Test ],
};
isa_ok($Test2, 'Test2');
isa_ok($Test2, 'Test');

is($Test2->test_CLASS,   $Test, '... got the right value back from the class method for $?CLASS');
is($Test2->test_PACKAGE, $Test, '... got the right value back from the class method for $?PACKAGE');
