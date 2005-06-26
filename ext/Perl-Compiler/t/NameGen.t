use v6;
use Test;

plan 6;

use_ok('Perl::Compiler::CodeGen::NameGen');

my $gen = ::Perl::Compiler::CodeGen::NameGen.new(template => { "REG$_" });
my $gen_foo = $gen.r('foo');
my $gen_bar = $gen.r('bar');
isnt($gen_foo, $gen_bar, 'Two separate names');
my $gen_foo2 = $gen.r('foo');
is($gen_foo, $gen_foo2, 'The same name generates the same register');

my $subgen = $gen.fork('down');
my $subgen_foo = $subgen.r('foo');
isnt($subgen_foo ne $gen_foo, 'Subgen has its own namespace');
isnt($subgen_foo ne $gen_bar, 'Subgen has its own namespace');
$subgen.ret($subgen_foo); #$subgen.r('foo'));
is($gen.r('down'), $subgen_foo, 'Return works properly');

# vim: ft=perl6 :
