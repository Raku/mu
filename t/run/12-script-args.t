use v6;

use Test;

plan 10;
if $*OS eq "browser" {
  skip_rest "Programs running in browsers don't have access to regular IO.";
  exit;
}

# Win9x breakage:
my $redir = "2>&1 >";

sub nonce () { return (".{$*PID}." ~ (1..1000).pick) }

sub run_test ($args) {
    my $out_fn = "temp-ex-output" ~ nonce;
    my $command = "$*EXECUTABLE_NAME $args $redir $out_fn";
    run $command;

    my $expected = "Unrecognized switch: -foo  (-h will show valid options).\n";
    my $got      = chomp( slurp $out_fn );
    unlink $out_fn;
    return $got;
}

is run_test("t/run/dump_params.pl 1"), ("1",).perl,     " bare arg works";
is run_test("t/run/dump_params.pl --n"), ("--n",).perl, " --n arg works";
for (qw/n p c e h V v/) {
    is run_test("t/run/dump_params.pl -$_"), ("-$_",).perl, " -$_ arg works";
}
is run_test("t/run/dump_params.pl -a"), ('-a',).perl, " -a arg works";
