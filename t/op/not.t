# taken mostly from Perl 5.8.7-tobe
use v6;

require Test;
plan 16;

# not() tests
# expected values for not() taken from 
# http://www.nntp.perl.org/group/perl.perl6.language/19397

my $not = not();
my @not = not();

ok ! defined $not, "not() returns undef in sclalar context";
ok ! $not, "not() returns false";

is ref @not, "Array", "not() returns Array in array context";
ok ! @not, "not() returns false";

# Various other checks for ! and not
ok (not 1) == (! 1), "Check not 1 == ! 1";
ok (not 0) == (! 0), "Check not 0 == ! 0";

# Test the not 1 equals the various falses. Check not 0 too.
# TODO: add undef below

my $not0 = not 0;
my $not1 = not 1;

ok $not1 eq '', "'' is false";
ok $not1 == (), "() is false";
ok $not1 == 0, "0 is false";
ok $not0 == 1, "1 is true";

# Test the ! 1 equals the various falses.  Check ! 0 too.
# TODO: add undef below

$not0 = ! 0;
$not1 = ! 1;

ok $not1 eq '', "'' is false";
ok $not1 == (), "() is false";
ok $not1 == 0, "0 is false";
ok $not0 == 1, "1 is true";
