use v6-alpha;

use Test;

plan 20;

=pod

Basic role tests from L<S12/"Roles">

=cut

# L<S12/"Roles">
# Basic definition
role Foo {}
class Bar does Foo;

ok Foo.HOW, "definition of a role worked";
ok Bar.HOW, "definition of a class which does a role worked";

# Smartmatch and .HOW.does
my $bar = Bar.new();
ok ($bar ~~ Bar),         '... smartmatch our $bar to the Bar class';
ok ($bar.HOW.does(Foo)), '.HOW.does said our $bar does Foo';
ok ($bar ~~ Foo),         'smartmatch said our $bar does Foo';

# Mixing a Role into an Object using imperative C<does>
my $baz = 3;
ok eval('$baz does Foo'),       'mixing in our Foo role into $baz worked', :todo<feature>;
ok eval('$baz.HOW.does(Foo)'), '.HOW.does said our $baz now does Foo', :todo<feature>;
ok eval('$baz ~~ Baz'),         'smartmatch said our $baz now does Foo', :todo<feature>;

# L<S12/"Roles" /but with a role keyword:/>
# Roles may have methods
ok eval('role A { method say_hello(Str $to) { "Hello, $to" } }'),
  "definition of a role with a method worked", :todo<feature>;
ok eval('(my Foo $a does A) .= new()'), 'mixing A into $a worked';
is eval('$a.say_hello("Ingo")'), "Ingo",
  'our $a "inherited" the .say_hello method of A', :todo<feature>;

# L<S12/"Roles" /Roles may have attributes:/>
ok eval('role B { has $.attr = 42 is rw }'),
  "definition of a role with an attribute worked", :todo<feature>;
ok eval('my Foo $b does B .= new()'), 'mixing B into $b worked';
is eval('$b.attr'), 42,      'our $b "inherited" the $.attr attribute of B (1)', :todo<feature>;
is eval('$b.attr = 23'), 23, 'our $b "inherited" the $.attr attribute of B (2)', :todo<feature>;

# L<S12/"Roles" /operator creates a copy and works on that./>
# As usual, ok instead of todo_ok to avoid unexpected succeedings.
ok eval('my Foo $c .= new()'),        'creating a Foo worked';
ok           !eval('$c ~~ B'),            '$c does not B';
ok eval('my $d = $c but B'),          'mixing in a Role via but worked', :todo<feature>;
ok           !eval('$c ~~ B'),            '$c still does not B...';
ok eval('$d ~~ B'),                   '...but $d does B', :todo<feature>;
