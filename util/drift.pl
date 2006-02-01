#!/usr/bin/perl
use IPC::Open2;
use FindBin qw<$Bin>;

-e "$Bin/../../DrIFT/src/DrIFT.hs" or exit;

# XXX - This is not at all portable.

$ENV{DERIVEPATH} = "$Bin/../src";

my ($in, $out) = @ARGV or exit;
my ($rh, $wh);
my $pid = open2(
    $rh, $wh,
    'runhugs',
    "-h1048576",
    "-P.:/usr/local/lib/hugs/libraries/:/usr/lib/hugs/libraries/:$Bin/../src/DrIFT:$Bin/../../DrIFT/src",
    "$Bin/../../DrIFT/src/DrIFT.hs",
    $in
);

my @program = do { <$rh> };
waitpid($pid, 0);
exit unless @program;

# Rearrange the DrIFT header
@program[0..2] = @program[2,0,1];

open OUT, "> $out" or die "Cannot open $out: $!";

for (@program) {
    next if /=begin DRIFT/ .. /=cut/;
    print OUT;
}
