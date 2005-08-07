#!/usr/bin/pugs
use Test;
plan 1;

# For tests of how Pugs behaves when given code with syntax errors.

# This test makes sure that Pugs's parser doesn't degenerate into extreme 
# slowness when it encounters an unterminated string with lots of interpolating 
# sequences in it.  (It does so as of 6 August 2005.)
sub time_eval($codestr) {
	my $start=time;
	eval $codestr;
	return time - $start;
}
my $short = time_eval q("$1$2$3$4       {}{}{}{}      );
my $long  = time_eval q("$1$2$3$4$5$6$7 {}{}{}{}{}{}{});
cmp_ok($long, &infix:«<=», $short * 10, "No degenerate case when parsing unterminated strings");
