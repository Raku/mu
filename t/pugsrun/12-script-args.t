#!/usr/bin/pugs

use v6;
use Test;
plan 10;

# Win9x breakage:
my ($pugs,$redir) = ("./pugs", "2>&1 >");

if($*OS eq any<MSWin32 mingw msys cygwin>) {
  $pugs = 'pugs.exe';
};

sub nonce () { return (".$*PID." ~ int rand 1000) }

sub run_test ($args) {
    my $out_fn = "temp-ex-output" ~ nonce;
    my $command = "$pugs $args $redir $out_fn";
    system $command;

    my $expected = "Unrecognized switch: -foo  (-h will show valid options).\n";
    my $got      = chomp( slurp $out_fn );
    unlink $out_fn;
    return $got;
}

is run_test("dump_params.p6 1"), "['1']",     " bare arg works";
is run_test("dump_params.p6 --n"), "['--n']", " --n arg works";
for (qw/n p c e h V v/) {
    is run_test("dump_params.p6 -$_"), "['-$_']", " -$_ arg works";
}
is run_test("dump_params.p6 -a"), "['-a']", " -a arg works";