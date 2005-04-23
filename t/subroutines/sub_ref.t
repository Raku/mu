#!/usr/bin/pugs

use v6;

require Test;

plan 27;
force_todo 5, 8, 10, 13, 15, 26, 27;

=head1 DESCRIPITION

These tests test subroutine references and their invocation.

See L<S06/"Types"> for more information about Code, Routine, Sub, Block, etc.

=cut

# See L<S06/"Types"> and especially L<A06/"The C<sub> form"> why {...} and ->
# ... {...} aren't Subs, but Blocks (they're all Codes, though).
# Quoting A06:
#                                   Code
#                        ____________|________________
#                       |                             |
#                    Routine                        Block
#       ________________|_______________            __|___
#      |     |       |       |    |     |          |      |
#     Sub Method Submethod Multi Rule Macro      Bare Parametric

{
    my $foo = sub () { 42 };
    isa_ok($foo, 'Code');
    isa_ok($foo, 'Routine');
    isa_ok($foo, 'Sub');
    is $foo.(), 42,                 "basic invocation of an anonymous sub";
    try { $foo.(23) };
    ok($!, "invocation of an parameterless anonymous sub with a parameter dies");
}

{
    my $foo = -> () { 42 };
    isa_ok($foo, 'Code');
    isa_ok($foo, 'Block');
    isa_ok($foo, 'Bare');
    is $foo.(), 42,                 "basic invocation of a pointy block";
    try { $foo.(23) };
    ok($!, "invocation of an parameterless pointy block with a parameter dies");
}

{
    my $foo = { 100 + $^x };
    isa_ok($foo, 'Code');
    isa_ok($foo, 'Block');
    isa_ok($foo, 'Parametric');
    is $foo.(42), 142,              "basic invocation of a pointy block with a param";
    try { $foo.() };
    ok($!, "invocation of an parameterized block expecting a param without a param dies");
}

{
    my $foo = sub { 100 + (@_[0] // -1) };
    isa_ok($foo, 'Code');
    isa_ok($foo, 'Routine');
    isa_ok($foo, 'Sub');
    is $foo.(42), 142,              "basic invocation of a perl5-like anonymous sub (1)";
    is $foo.(),    99,              "basic invocation of a perl5-like anonymous sub (2)";
}

{
    my $foo = sub ($x) { 100 + $x };
    isa_ok($foo, 'Code');
    isa_ok($foo, 'Routine');
    isa_ok($foo, 'Sub');
    is $foo.(42),      142,    "calling an anonymous sub with a positional param";
    is $foo.(x => 42), 142,    "calling an anonymous sub with a positional param addressed by name";
    try{ $foo.() };
    ok($!, "calling an anonymous sub expecting a param without a param dies");
    try{ $foo.(42, 5) };
    ok($!, "calling an anonymous sub expecting one param with two params dies");
}
