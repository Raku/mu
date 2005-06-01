#!/usr/bin/pugs

use v6;
use Test;

plan 16;

# ref() on basic types

my $a;
isa_ok($a, 'Any', 'it is an Any type');

my @a;
isa_ok(@a, 'Array', 'it is an Array type');

my %a;
isa_ok(%a, 'Hash', 'it is an Hash type');

# ref() on reference types

my $b1 = [];
isa_ok($b1, 'List', 'it is a List ref type');

# this seems to be the only way to make a hash - ref at the moment
my %b2 = ("one", 1); my $b2 = %b2;
isa_ok($b2, 'Hash', 'it is a Hash ref type'); 

# ref() on subroutines

my $s1 = sub {};
isa_ok($s1, 'Sub', 'it is a Sub type');

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

# L<S06/"Types" /Bare\s*Basic Perl block/>
my $s2 = {};
isa_ok($s2, 'Bare', 'it is a Sub type (bare block)');

# L<S06/"Types" /Parametric\s+Basic Perl block with placeholder parameters/>
my $s2a = { $^a };
isa_ok($s2a, 'Parametric', 'it is a Parametric type (bare block with placeholder parameters)', :todo);

my $s3 = -> {};
isa_ok($s3, 'Block', 'it is a Block type (pointy block)');

# ref() on different types of scalars

my $int = 0;
isa_ok($int, 'Int', 'it is an Int type');

# the only way I can seem to get Num is to force numeric context
my $num = '';
isa_ok(+$num, 'Num', 'it is an Num type');

my $float = 0.5;
isa_ok($float, 'Rat', 'it is an Rat type');

my $string = "Hello World";
isa_ok($string, 'Str', 'it is a Str type');

my $bool = (0 == 0);
isa_ok($bool, 'Bool', 'it is a Bool type');

my $pair = "foo" => "bar";
isa_ok($pair, 'Pair', 'it is a Pair type');

my $rule = rx:perl5{^hello\sworld$};
isa_ok($rule, 'Rule', 'it is a Rule type');
