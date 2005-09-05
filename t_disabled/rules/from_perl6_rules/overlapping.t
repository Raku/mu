#!/usr/bin/pugs

use v6;
use Test;

=pod

This file was derived from the perl5 CPAN module Perl6::Rules,
version 0.3 (12 Apr 2004), file t/overlapping.t.

It has (hopefully) been, and should continue to be, updated to
be valid perl6.

=cut

plan 10;

if(!eval('("a" ~~ /a/)')) {
  skip_rest "skipped tests - rules support appears to be missing";
} else {

my $str = "abrAcadAbbra";

my @expected = (
    [ 0 => 'abrAcadAbbra' ],
    [ 3 =>    'AcadAbbra' ],
    [ 5 =>      'adAbbra' ],
    [ 7 =>        'Abbra' ],
);

for (1..2) -> $rep {
     ok($str ~~ m:i:overlap/ a .+ a /, "Repeatable overlapping match ($rep)" );

    ok(@$/ == @expected, "Correct number of matches ($rep)" );
    my %expected; %expected{map {$_[1]}, @expected} = (1) x @expected;
    my %position; %position{map {$_[1]}, @expected} = map {$_[0]}, @expected;
    for (@$/) {
        ok( %expected{$_}, "Matched '$_' ($rep)" );
        ok( %position{$_} == $_.pos, "At correct position of '$_' ($rep)" );
        delete %expected{$_};
    }
    ok(%expected.keys == 0, "No matches missed ($rep)" );
}
 
ok(!( "abcdefgh" ~~ m:overlap/ a .+ a / ), 'Failed overlapping match');
ok(@$/ == 0, 'No matches');

ok($str ~~ m:i:overlap/ a (.+) a /, 'Capturing overlapping match');

ok(@$/ == @expected, 'Correct number of capturing matches');
my %expected; %expected{@expected} = (1) x @expected;
for (@$/) {
    my %expected; %expected{map {$_[1]}, @expected} = (1) x @expected;
    ok( $_[1] = substr($_[0],1,-1), "Captured within '$_'" );
    delete %expected{$_};
}

}

