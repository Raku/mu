#!/usr/bin/pugs

use v6;
use Test;

plan 22;

=pod

Parameterized role tests, see L<S12/"Roles">

=cut

# L<S12/"Roles" /to be considered part of the long name:/>
# L<A12/"Encapsulated Attributes" /to be considered part of the long name:/>
ok eval('
  role InitialAttribVal[: $val] {
    has $.attr = $val;
  }
  1
'), "parameterized role definition (1)", :todo<feature>;

my $a;
ok eval('$a does InitialAttribVal[42]'),
  "imperative does to apply a parametrized role (1)", :todo<feature>;
is try { $a.attr }, 42,
  "attribute was initialized correctly (1)", :todo<feature>;
# L<A12/"Encapsulated Attributes" /In which case all of these are true:/>
ok eval('$a.meta.does(InitialAttribVal)'),
  ".meta.does gives correct information (1-1)", :todo<feature>;
# L<A12/"Encapsulated Attributes" /but this is false:/>
ok eval('!$a.meta.does(InitialAttribVal[42])'),
  ".meta.does gives correct information (1-2)", :todo<feature>;

my $b;
ok eval('$a does InitialAttribVal[23]'),
  "imperative does to apply a parametrized role (2)", :todo<feature>;
is try { $a.attr }, 23,
  "attribute was initialized correctly (2)", :todo<feature>;
# L<A12/"Encapsulated Attributes" /In which case all of these are true:/>
ok eval('$a.meta.does(InitialAttribVal)'),
  ".meta.does gives correct information (2-1)", :todo<feature>;
# L<A12/"Encapsulated Attributes" /but this is false:/>
ok eval('!$a.meta.does(InitialAttribVal[23])'),
  ".meta.does gives correct information (2-2)", :todo<feature>;



# L<A12/"Parametric types" /but you can also parameterize other types explicitly:/>
# L<S12/"Roles" /A role's main type is generic by default/>
eval_ok '
  role InitialAttribType[^vartype:] {
    method hi(vartype $foo) { 42 }
  }
', "parameterized role definition (2)", :todo<feature>;
my $c;
ok eval('$c does InitialAttribType[Code]'),
  "imperative does to apply a parametrized role (3)", :todo<feature>;
ok eval('$c.meta.does(InitialAttribType)'),
  ".meta.does gives correct information (3-1)", :todo<feature>;
ok eval('$c.meta.does(InitialAttribType[Code])'),
  ".meta.does gives correct information (3-2)", :todo<feature>;
is try { $c.hi(sub {}) }, 42,
  "type information was processed correctly (1)", :todo<feature>;
dies_ok { $c.hi("not a code object") },
  "type information was processed correctly (2)";




# Parameterized role using both a parameter which will add to the "long name"
# of the role and one which doesn't.
# (Explanation: This one is easier. The two attributes $.type and $.name will
# be predefined (using the role parameterization). The $type adds to the long
# name of the role, $name does not. Such:
#   my $a does InitialAttribBoth["foo", "bar"];
#   my $b does InitialAttribBoth["foo", "grtz"];
#   $a ~~ InitialAttribBoth                ==> true
#   $b ~~ InitialAttribBoth                ==> true
#   $a ~~ InitialAttribBoth["foo"]         ==> true
#   $b ~~ InitialAttribBoth["foo"]         ==> true
#   $a ~~ InitialAttribBoth["foo", "bar"]  ==> false
#   $b ~~ InitialAttribBoth["foo", "grtz"] ==> false
# Heavy stuff, eh?)
ok eval('
  role InitialAttribBoth[Str $type: Str $name] {
    has $.type = $type;
    has $.name = $name;
  }
  1
'), "parameterized role definition (3)", :todo<feature>;
my $d;
ok eval('$d does InitialAttribBoth["type1", "name1"]'),
  "imperative does to apply a parametrized role (4)", :todo<feature>;
ok eval('$c.meta.does(InitialAttribType)'),
  ".meta.does gives correct information (4-1)", :todo<feature>;
ok eval('$d.meta.does(InitialAttribType["type1"])'),
  ".meta.does gives correct information (4-2)", :todo<feature>;
ok eval('!$d.meta.does(InitialAttribType["type1", "name1"])'),
  ".meta.does gives correct information (4-3)", :todo<feature>;
is try { $d.type }, "type1", ".type works correctly", :todo<feature>;
is try { $d.name }, "name1", ".name works correctly", :todo<feature>;
