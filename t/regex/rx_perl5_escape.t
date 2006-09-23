use v6-alpha;

use Test;

plan 1;

unless "a" ~~ rx:P5/a/ {
  skip_rest "skipped tests - P5 regex support appears to be missing";
  exit;
}

my $str = "http://foo.bar/";
ok(($str ~~ rx:perl5/http:\/\//), "test the regular expression escape");

