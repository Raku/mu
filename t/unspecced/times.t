use v6;

use Test;

plan 5;

my $FUDGE = 5000;      # will need to be updated as pugs get faster
my @start = times();
for (1..$FUDGE) {
    1+1;
}
my @end = times();
ok(@end[0] > @start[0], 'something changed in times()');
diag "end: {@end} start: {@start}";

cmp_ok(@end[0] - @start[0], &infix:«<=», 10, 'sensible time spent')
  or do {
    diag "Start: "~ @start;
    diag "End:   "~ @end;  
  };

my $start = time();
my $stop = $start+2;
@start = times();
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

cmp_ok(@diff[0], &infix:«<», 3, "Spent more than 2 seconds in user space")
  or do {
    diag "Start: "~ @start;
    diag "End:   "~ @end;
  };

is(@diff[1..3].map:({ $_ < 1 ?? 0 !! $_ }), [0,0,0], "Two successive calls spend zero time in IO")
  or do {
    diag "Start: "~ @start;
    diag "End:   "~ @end;
  };
