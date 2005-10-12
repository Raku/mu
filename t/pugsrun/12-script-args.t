#!/usr/bin/pugs

use v6;
use Test;

plan 10;
if $*OS eq "browser" {
  skip_rest "Programs running in browsers don't have access to regular IO.";
  exit;
}

# Win9x breakage:
my ($pugs,$redir) = ("../../pugs", "2>&1 >");

if($*OS eq any<MSWin32 mingw msys cygwin>) {
  $pugs = '..\\..\\pugs.exe';
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

chdir "t"; chdir "pugsrun";
is run_test("dump_params.p6 1"), ("1",).perl,     " bare arg works";
is run_test("dump_params.p6 --n"), ("--n",).perl, " --n arg works";
for (qw/n p c e h V v/) {
    is run_test("dump_params.p6 -$_"), ("-$_",).perl, " -$_ arg works";
}
is run_test("dump_params.p6 -a"), ('-a',).perl, " -a arg works";
