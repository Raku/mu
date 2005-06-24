#!/usr/bin/pugs

use v6;
use Test;

plan 18;

=pod

Very basic class tests from L<S12/"Classes">

=cut

# L<S12/"Classes">

class Foo {};

my $foo = Foo.new();
ok($foo ~~ Foo, '... smartmatch our $foo to the Foo class', :todo<feature>);

# note that S12 says that .isa() should be called on metaclasses.
# However, making it an object .isa() means that classes are free to
# override the behaviour without playing with the metamodel via traits
ok($foo.isa(Foo), '.isa(Foo)');
ok($foo.isa(::Foo), '.isa(::Foo)');
ok($foo.isa("Foo"), '.isa("Foo")');
ok(!$foo.isa("Bar"), '!.isa("Bar")');

my $foo_clone = $foo.clone();
ok($foo_clone ~~ Foo, '... smartmatch our $foo_clone to the Foo class', :todo<feature>);

class Foo::Bar {};

my $foo_bar = Foo::Bar.new();
ok($foo_bar ~~ Foo::Bar, '... smartmatch our $foo_bar to the Foo::Bar class', :todo<feature>);

ok($foo_bar.isa(Foo::Bar), '.isa(Foo::Bar)');
ok(!$foo.isa(::Foo), 'Foo::Bar.new.isa(::Foo)');

# L<S12/"Classes" /An \"isa\" is just a trait that happens to be another class\:/>

class Bar is Foo {};

ok(Bar ~~ Foo, '... smartmatch our Bar to the Foo class');

my $bar = Bar.new();
ok($bar ~~ Bar, '... smartmatch our $bar to the Bar class', :todo<feature>);
ok($bar.isa(Bar), "... .isa(Bar)");
ok($bar ~~ Foo, '... smartmatch our $bar to the Foo class', :todo<feature>);
ok($bar.isa(Foo), "new Bar .isa(Foo)", :todo<bug>);

my $bar_clone = $bar.clone();
ok($bar_clone ~~ Bar, '... smartmatch our $bar_clone to the Bar class', :todo<feature>);
ok($bar_clone.isa(Bar), "... .isa(Bar)");
ok($bar_clone ~~ Foo, '... smartmatch our $bar_clone to the Foo class', :todo<feature>);
ok($bar_clone.isa(Foo), "... .isa(Foo)", :todo<bug>);
