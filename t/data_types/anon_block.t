#!/usr/bin/pugs

use v6;
require Test;

=kwid

Block tests

This covers anonymous blocks and subs, as well as pointy blocks
(with and without args) and bare blocks. 

L<S06/"Blocks">
L<S04/"The Relationship of Blocks and Declarations">

=cut

plan 25;
force_todo 14, 15;

# anon blocks L<S06/"Standard Subroutines">
my $anon_sub = sub { 1 };
isa_ok($anon_sub, 'Sub');
is($anon_sub(), 1, 'sub { } works');

my $anon_sub_w_arg = sub ($arg) { 1 + $arg };
isa_ok($anon_sub_w_arg, 'Sub');
is($anon_sub_w_arg(3), 4, 'sub ($arg) {} works');

# anon blocks L<S06/"Blocks">
my $anon_block = { 1 };
isa_ok($anon_block, 'Block');
is($anon_block(), 1, '{} <anon block> works');

# pointy subs L<S06/"Pointy subs">
my $pointy_block = -> { 1 };
isa_ok($pointy_block, 'Block');
is($pointy_block(), 1, '-> {} <"pointy" block> works');

my $pointy_block_w_arg = -> $arg { 1 + $arg };
isa_ok($pointy_block_w_arg, 'Block');
is($pointy_block_w_arg(3), 4, '-> $arg {} <"pointy" block w/args> works');

my $pointy_block_w_multiple_args = -> $arg1, $arg2 { $arg1 + $arg2 };
isa_ok($pointy_block_w_multiple_args, 'Block');
is($pointy_block_w_multiple_args(3, 4), 7, '-> $arg1, $arg2 {} <"pointy" block w/multiple args> works');

my $pointy_block_nested = -> $a { -> $b { $a + $b }};
isa_ok($pointy_block_nested, 'Block');
isa_ok($pointy_block_nested(5), 'Block'); 
eval_is('$pointy_block_nested(5)(6)', 11, '-> $a { -> $b { $a+$b }} nested <"pointy" block> works'); 

# bare blocks L<S06/"Blocks">

my $foo;
eval '{$foo = "blah"};';
is($foo, "blah", "lone block actually executes it's content");

my $foo2;
eval '{$foo2 = "blah"}';
is($foo2, "blah", "lone block w/out a semicolon actually executes it's content");

my ($one, $two);
eval '{$one = 1} {$two = 2}';
is($one, undef, 'two blocks ({} {}) no semicolon after either,.. first block does not execute');
is($two, 2, '... but second block does (parsed as hash subscript)');

my ($one_a, $two_a);
eval '{$one_a = 1}; {$two_a = 2}';
is($one_a, 1, '... two blocks ({}; {}) semicolon after the first only,.. first block does execute');
is($two_a, 2, '... and second block does too');

my ($one_b, $two_b);
eval '
{
    $one_b = 1
}
{
    $two_b = 2
}';
is($one_b, 1, '... two stand-alone blocks ({\n...\n}\n{\n...\n}),.. first block does execute');
is($two_b, 2, '... and second block does too');

my ($one_c, $two_c);
eval '{$one_c = 1}; {$two_c = 2};';
is($one_c, 1, '... two blocks ({}; {};) semicolon after both,.. first block does execute');
is($two_c, 2, '... and second block does too');
