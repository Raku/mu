use v6;

use Test;

plan 11;

=begin pod

Very basic meta-class tests from L<S12/Introspection>

=end pod

class Foo-0.0.1 { method bar ($param) returns Str { return "baz" ~ $param } };

# L<S12/Introspection/should be called through the meta object>

ok(eval("Foo.HOW.can('bar')"), '... Foo can bar', :todo<feature>);
ok(eval("Foo.^can('bar')"), '... Foo can bar (as class method)', :todo<feature>);
ok(eval("Foo.HOW.isa(Foo)"), '... Foo is-a Foo (of course)', :todo<bug>);
ok(eval("Foo.^isa(Foo)"), '... Foo is-a Foo (of course) (as class method)', :todo<bug>);

# L<S12/Introspection/Class traits may include:>

ok(eval("Foo.HOW.name() eq 'Foo'"), '... the name() property is Foo');
ok(eval("Foo.HOW.version() == 0.0.1"), '... the version() property is 0.0.1', :todo<feature>);
ok(eval("(Foo.HOW.isa())[0] ~~ Foo"), '... the isa() property returns Foo as the first parent class', :todo<feature>);

# L<S12/Introspection/The .HOW.methods method returns method-descriptors containing:>

# NOTE: I am guessing on some of this here, but it's a start for now

my @methods = eval 'Foo.HOW.methods()';
is(eval("@methods[0].name eq 'bar'"), '... our first method is foo()', :todo<feature>);
is(eval("@methods[0].signature eq '\$param'"), '... our first methods signature is $param', :todo<feature>);
is(eval("@methods[0].returns ~~ Str"), '... our first method returns a Str', :todo<feature>);
ok(eval("!@methods[0].multi"), '... our first method is not a multimethod', :todo<feature>);

