# translated from CPAN module Getopt::Long 2.34
# http://search.cpan.org/src/JV/Getopt-Long-2.34/t/gol-linkage.t
use v6;
use Test;
plan 18;

use Getopt::Long;

@*ARGS = <-Foo -baR --foo bar>;
Getopt::Long::Configure<no_ignore_case>;
my $options;
ok(GetOptions(\$options, 'foo', 'Foo=s'),
                                       'call to GetOptions with hash '~
                                       'as first argument');
ok(exists $options<foo>,               'foo is a key in $options');
is(+$options<foo>, 1,                  'the value of $options<foo> is 1');
ok(exists $options<Foo>,               'Foo is a key in $options');
is($options<Foo>, '-baR',              'the value of $options<Foo> is "-baR"');
is(@*ARGS.elems, 1,                    'only 1 element left in @*ARGS');
is(@*ARGS[0], 'bar',                   'the remaining element is "bar"');
ok(not exists $options<baR>,           'baR is not a key in $options');
ok(not exists $options<bar>,           'bar is not a key in $options');

@*ARGS = <-Foo -baR --foo bar>;
Getopt::Long::Configure<default, no_ignore_case>;
my $foo;
ok(GetOptions(\$options, 'foo' => \$foo, 'Foo=s'),
                                       'call to GetOptions with hash '~
                                       'and pair');
ok(exists $options<foo>,               'foo is a key in $options');
is(+$foo, 1,                           'the value of $foo is 1');
ok(exists $options<Foo>,               'Foo is a key in $options');
is($options<Foo>, '-baR',              'the value of $options<Foo> is "-baR"');
is(@*ARGS.elems, 1,                    'only 1 element left in @*ARGS');
is(@*ARGS[0], 'bar',                   'the remaining element is "bar"');
ok(not exists $options<foo>,           'foo is not a key in $options');
ok(not exists $options<baR>,           'baR is not a key in $options');
ok(not exists $options<bar>,           'bar is not a key in $options');
