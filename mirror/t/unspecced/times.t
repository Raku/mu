#!/usr/bin/pugs

use v6;
use Test;

plan 5;

my @start = times();
for (1..1000) {
    1+1;
}
my @end = times();
ok(@end[0] > @start[0], 'something changed in times()');

cmp_ok(@end[0] - @start[0], &infix:<<=>, 10, 'sensible time spent')
  or do {
    diag "Start: "~ @start;
    diag "End:   "~ @end;  
  };

my $start = time();
my $stop = $start+1;
@start = times();
my $stop = $start+2;
diag "Looping 2 seconds";
my $curr = time();
while ($stop > $curr) {
  $curr = time(); 
}
my $end = time();
@end = times();
my @diff = @end >>-<< @start;

my $total = [+] @diff;
ok( $total <= $end - $start, "Total time accounted is less than or equal wallclock time");

cmp_ok(@diff[0], &infix:<<=>, 2, "Spent more than 2 seconds in user space")
  or do {
    diag "Start: "~ @start;
    diag "End:   "~ @end;
  };

is(@diff[1..3].map:{ $_ <= 0.0001 ?? 0 :: $_ },[0,0,0], "Two successive calls spend zero time in IO")
  or do {
    diag "Start: "~ @start;
    diag "End:   "~ @end;
  };
