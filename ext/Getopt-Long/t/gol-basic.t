# translated from CPAN module Getopt::Long 2.34
# http://search.cpan.org/src/JV/Getopt-Long-2.34/t/gol-basic.t
use v6;
use Test;
plan 9;

use Getopt::Long <:config no_ignore_case>;

@*ARGS = <-Foo -baR --foo bar>;
my $options;
ok($options = GetOptions ("foo", "Foo=s"));
ok(exists $options<foo>);
is(+$options<foo>, 1);
ok(exists $options<Foo>);
is($options<Foo>, '-baR');
is(@*ARGS.elems, 1);
is(@*ARGS[0], 'bar');
ok(not exists $options<baR>);
ok(not exists $options<bar>);
