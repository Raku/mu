# translated from CPAN module Getopt::Long 2.34
# http://search.cpan.org/src/JV/Getopt-Long-2.34/t/gol-basic.t
use v6;
use Test;
plan 9;

use Getopt::Long <:config no_ignore_case>;

@*ARGS = <-Foo -baR --foo bar>;
my $options;
ok($options = GetOptions <foo, Foo=s>, 'call to GetOptions');
ok(exists $options<foo>,               'foo is a key in $options');
is(+$options<foo>, 1,                  'the value of $options<foo> is 1');
ok(exists $options<Foo>,               'Foo is a key in $options');
is($options<Foo>, '-baR',              'the value of $options<foo> is "-baR"');
is(@*ARGS.elems, 1,                    'only 1 element left in @*ARGS');
is(@*ARGS[0], 'bar',                   'the remaining element is "bar"');
ok(not exists $options<baR>,           'baR is not a key in $options');
ok(not exists $options<bar>,           'bar is not a key in $options');
