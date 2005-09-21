# translated from CPAN module Getopt::Long 2.34
# http://search.cpan.org/src/JV/Getopt-Long-2.34/t/gol-basic.t
use v6;
use Test;
plan 9;

use Getopt::Long <:config no_ignore_case>;

@*ARGS = <-Foo -baR --foo bar>;
undefine $opt_baR;
undefine $opt_bar;
ok(GetOptions ("foo", "Foo=s"));
ok(defined $opt_foo);
is(+$opt_foo, 1);
ok(defined $opt_Foo);
is($opt_Foo, '-baR');
is(@ARGV.elems, 1);
is(@ARGV[0], 'bar');
ok(not defined $opt_baR);
ok(not defined $opt_bar);
