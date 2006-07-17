use v6-alpha;

use Test;

plan 14;

my @a = (1, 2, 3);
ok(@a .= map:{ $_ + 1 }, '.= parses with adverbial block');
is(@a[0], 2, 'inplace map [0]');
is(@a[1], 3, 'inplace map [1]');
is(@a[2], 4, 'inplace map [2]');

my @b = <foo 123 bar 456 baz>;
ok(eval('@b.=grep:{/[a-z]/}'), '.= parses without surrounding whitespace',
   :todo<bug>);
is @b[0], 'foo', 'inplace grep [0]';
is @b[1], 'bar', 'inplace grep [1]', :todo<bug>;
is @b[2], 'baz', 'inplace grep [2]', :todo<bug>;

my $a=3.14;
$a .= int;
is($a, 3, "inplace int");

my $b = "a_string"; $b .= ref;
my $c =         42; $c .= ref;
my $d =      42.23; $d .= ref;
my @e = <a b c d>;  @e .= ref;
is($b,    "Str",   "inplace ref of a Str");
is($c,    "Int",   "inplace ref of a Num");
is($d,    "Rat",   "inplace ref of a Rat");
is(@e[0], "Array", "inplace ref of an Array");

my $f = "lowercase"; $f .= uc;
my $g = "UPPERCASE"; $g .= lc;
my $h = "lowercase"; $h .= ucfirst;
my $i = "UPPERCASE"; $i .= lcfirst;
is($f, "LOWERCASE", "inplace uc");
is($g, "uppercase", "inplace lc");
is($h, "Lowercase", "inplace ucfist");
is($i, "uPPERCASE", "inplace lcfirst");

# L<S12/"Mutating methods">
my @b = <z a b d e>;
@b .= sort;
is ~@b, "a b d e z", "inplace sort";
