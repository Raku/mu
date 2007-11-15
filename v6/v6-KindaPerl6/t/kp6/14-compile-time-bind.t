use v6-alpha;

module Main {

say '1..1';

my $counter ::= 1;
my $v;

BEGIN {
  $v = $counter;
};
say "ok ", $v;

}
