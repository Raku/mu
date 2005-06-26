
# this test tests that the output (text output and return code) of
# test scripts are correct.

use v6;
use Test;

plan 1;

# this test tests that various failure conditions (that we don't want
# to show up as failures) happen, and test the the output of the test
# suite is correct.

# ... copied from t/pugsrun/05-unknown-option.t, but it looks wrong :)
sub nonce () { return (".$*PID." ~ int rand 1000) }
my $out_fn = "temp-ex-output" ~ nonce;
my $redir = "2>&1 >";
if($*OS eq any<MSWin32 mingw msys cygwin>) {
    $redir = ">";
};

my $cmd = "$*EXECUTABLE_NAME t/3-script.pl $redir $out_fn";

diag($cmd);
system($cmd);

my $output = slurp $out_fn;
unlink($out_fn);

# currently this output exposes one bug - the trailing space
is($output, "1..1
ok 1 - TODO that passes # TODO 
", "got correct output");

