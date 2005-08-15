#!/usr/bin/perl
use IPC::Open2;
use FindBin qw<$Bin>;

# XXX - This is not at all portable.

my ($rh, $wh);
my $pid = open2(
    $rh, $wh,
    'runhugs',
    "-P.:/usr/local/lib/hugs/libraries/:/usr/lib/hugs/libraries/:$Bin/../src/DrIFT:$Bin/../../DrIFT/src",
    "$Bin/../../DrIFT/src/DrIFT.hs",
    @ARGV
);

my @program = do { <$rh> };
waitpid($pid, 0);
exit 1 unless @program;

# Rearrange the DrIFT header
@program[0..2] = @program[2,0,1];

for (@program) {
    next if /=begin DRIFT/ .. /=cut/;
    print;
}
