use FindBin '$Bin';
use lib
    "$Bin/lib",
    "$Bin/../../../../lib",
    "$Bin/../Pugs-Compiler-Rule/lib",
    "$Bin/../Pugs-Utils/lib",
    "$Bin/../Pugs-Compiler-Precedence/lib",
;

use Pugs::Grammar::Perl6;
use Pugs::Utils::Dump;
use strict;
use warnings;

if ( @ARGV ) {
    my @a=<>;
    my $src = join('', @a);
    my $match = Pugs::Grammar::Perl6->parse( $src );
    #use YAML;
    #print Dump $match->();
    print dump_tree $match->();
    print "tail: ", substr( ${$match}->{tail}, 0, 20 ),"...\n";
    exit;
}

#use Test::More 'no_plan';
print q(#if key:<val> {10 + $a / "abc"}),"\n";
my $match = Pugs::Grammar::Perl6->parse(<<'PERL6');
if key:<val> {
    10 + $a / "abc"
}
$string.isa("Str");
$string.isa "Str";
#$string.say;


{
    my $string = "Pugs";
    if $string.isa("Str") { say "ok 1" } else { say "not ok 1" }
}

{
    my $num = 3.141;
    if $num.isa("Num")    { say "ok 2" } else { say "not ok 2" }
}

#~ {
    #~ my $string = "Pugs";
    #~ if $string.ref eq "Str" { say "ok 1" } else { say "not ok 1" }
#~ }

sub foo () {
    say "ok";
}

print &sub123;

$bar.goto("param1", "param2");
&bar.goto("param1", "param2");
subname;
subname();
subname 99;
$bar.methodname 99;
$bar.methodname;
my $code = { 42 };

sub ok_auto {
    say "ok $counter";
}

module Test-0.0.6;
$Test::ALWAYS_CALLER = %ENV<TEST_ALWAYS_CALLER>;
$Test::num_of_tests_run    = 0;
$Test::num_of_tests_planned;
say <angle quoted>;
say 1 < 2;
1;
PERL6

    print dump_tree $match->();
    print "tail: ", substr( ${$match}->{tail}, 0, 20 ),"...\n";

