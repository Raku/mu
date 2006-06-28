use FindBin '$Bin';
use lib
    "$Bin/../lib",
    # "$Bin/../../../../lib",
    "$Bin/../../Pugs-Compiler-Rule/lib",
    "$Bin/../../Pugs-Compiler-Precedence/lib",
;

use Pugs::Grammar::Perl6;
use strict;
use warnings;
use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Sortkeys = 1;

if ( @ARGV ) {
    my @a=<>;
    my $src = join('', @a);
    my $match = Pugs::Grammar::Perl6->parse( $src );
    #use YAML;
    #print Dump $match->();
    print Dumper $match->();
    print "tail: ", substr( ${$match}->{tail}, 0, 20 ),"...\n";
    exit;
}

#use Test::More 'no_plan';
print q(#if key:<val> {10 + $a / "abc"}),"\n";
my $match = Pugs::Grammar::Perl6->parse(<<'PERL6');
say 1 ?? 2 !! 3;

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

sub plan (Int $number_of_tests, :$x, $y? ) returns Void is export {
    say "1..$number_of_tests";
}

1 if 4; 2 if 5;
my $x = do 1 if 2;

say(3 if 4);    # wrong?
(4 if 5) + (6 if 7);   # wrong?

#1 if 2 if 3;

$*ERR.say("# ok");
END { Test::test_ends() }
1;
=kwid
=cut
say;
return if 1;

PERL6

    print Dumper $match->();
    print "tail: ", substr( ${$match}->{tail}, 0, 20 ),"...\n" if ${$match}->{tail};
__END__

use YAML;
print Dump $match->();

