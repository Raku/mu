use v6;
use Test;

plan 3;

# First 2 fail regardless of PGE or Pugs::Compiler::Rule
# Last one shows that changing the whitspace to a char class succeeds

my $str1 = "Hello world";
$str1 ~~ s :s/Hello (\w+)/foo/;
is($str1, 'foo', "white space works in subst");

my $str3 = "Hello world";
$str3 ~~ s/Hello (\w+)/foo/;
is($str3, 'foo', "white space works in subst");

my $str4 = "Hello world";
$str4 ~~ s/Hello\s(\w+)/foo/;
is($str4, 'foo', "white space works in subst");
