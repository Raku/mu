#!/usr/bin/pugs

use v6;
use Test;

plan 6;

=pod

Really really really minimal s:perl5//// tests.

Please add more!!

=cut

unless "a" ~~ rx:P5/a/ {
  skip_rest "skipped tests - P5 regex support appears to be missing";
  exit;
}

my $foo = "foo";
$foo ~~ s:perl5{f}{b};
is($foo, "boo", 'substitute regexp works');
unless $foo eq "boo" {
  skip_rest "Skipping test which depend on a previous failed test";
}

my $bar = "barrrr";
$bar ~~ s:perl5:g{r+}{z};
is($bar, "baz", 'substitute regexp works with :g modifier');

my $path = "/path//to///a//////file";
$path ~~ s:perl5:g{/+}{/};
is($path, "/path/to/a/file", 'substitute regexp works with :g modifier');

my $baz = "baz";
$baz ~~ s:perl5{.(a)(.)}{$1$0p};
is($baz, "zap", 'substitute regexp with capturing variables works');

my $bazz = "bazz";
$bazz ~~ s:perl5:g{(.)}{x$0};
is($bazz, "xbxaxzxz", 'substitute regexp with capturing variables works with :g');

my $bad = "1   ";
$bad ~~ s:perl5:g/\s*//;
is($bad, "1", 'Zero width replace works with :g');
