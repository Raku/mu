# tsanta.pl. Santa Claus golf game test program.
# For more detail: http://www.perlmonks.org/?node_id=438876
# For historical interest, I've left this program pretty much
# unchanged from the original p5 Santa golf game of 2001.
# Also for future historical amusement, I've inserted unchanged
# my original first attempts, head.p6, tail.p6, rev.p6, mid.p6,
# wc.p6, developed with a very early version of Pugs (6.0.10)
# and with very little knowledge of Perl 6. These five examples
# can be improved in many ways, and have been in the Perl Monks
# thread mentioned above.
#
# To test head.p6, tail.p6, rev.p6, mid.p6, wc.p6 for correctness,
# simply run this program in the same directory containing
# those files.

use strict;

sub GolfScore {
   my $script = shift;
   open(FF, $script) or die "error: open '$script'";
   my $golf = 0;
   while (<FF>) {
      chomp; next unless length;
      s/^#!.*?perl// if $. == 1;
      $golf += length;
   }
   close(FF);
   return $golf;
}

sub PrintGolfScore {
   my @scr = @_;
   my $tot = 0;
   for my $s (@scr) { $tot += GolfScore($s) }
   print "You shot a round of $tot strokes.\n";
}

sub BuildFile {
   my ($fname, $data) = @_;
   open(FF, '>'.$fname) or die "error: open '$fname'";
   print FF $data;
   close(FF);
}

sub CheckOne {
   my ($scr, $label, $data, $exp) = @_;
   my $intmp  = 'in.tmp';
   BuildFile($intmp, $data);
   my $cmd = "pugs $scr $intmp";
   print "$label: running: '$cmd'...";
   my $out = `$cmd`; my $rc = $? >> 8;
   print "done (rc=$rc).\n";
   if ($out ne $exp) {
      warn "Expected:\n"; print STDERR $exp;
      warn "Got:\n"; print STDERR $out;
      die "Oops, you failed.\n";
   }
}

# -----------------------------------------------------

my $file1 = <<'GROK';
1st line
GROK

my $file2 = <<'GROK';
1st line
2nd line
GROK

my $file3 = <<'GROK';
1st line
2nd line
3rd line
GROK

my $file4 = <<'GROK';
1st line
2nd line
3rd line
4th line
GROK

my $file12 = <<'GROK';
1st line
2nd line
3rd line
4th line
5th line
6th line
7th line
8th line
9th line
10th line
11th line
12th line
GROK

my $file21 = <<'GROK';
1st line
2nd line
3rd line
4th line
5th line
6th line
7th line
8th line
9th line
10th line
11th line
12th line









GROK

# -----------------------------------------------------

sub CheckHead {
   my ($scr) = @_;
   my @tt = (
      [ 'file1',  $file1,  "1st line\n" ],
      [ 'file2',  $file2,  "1st line\n2nd line\n" ],
      [ 'file3',  $file3,  "1st line\n2nd line\n3rd line\n" ],
      [ 'file12', $file12,
        "1st line\n2nd line\n3rd line\n4th line\n5th line\n".
        "6th line\n7th line\n8th line\n9th line\n10th line\n" ],
   );
   for my $r (@tt) { CheckOne($scr, $r->[0], $r->[1], $r->[2]) }
}

sub CheckTail {
   my ($scr) = @_;
   my @tt = (
      [ 'file1',  $file1,  "1st line\n" ],
      [ 'file2',  $file2,  "1st line\n2nd line\n" ],
      [ 'file3',  $file3,  "1st line\n2nd line\n3rd line\n" ],
      [ 'file12', $file12,
        "3rd line\n4th line\n5th line\n6th line\n7th line\n".
        "8th line\n9th line\n10th line\n11th line\n12th line\n" ],
      [ 'file21', $file21, "12th line\n\n\n\n\n\n\n\n\n\n" ],
   );
   for my $r (@tt) { CheckOne($scr, $r->[0], $r->[1], $r->[2]) }
}

sub CheckRev {
   my ($scr) = @_;
   my @tt = (
      [ 'file1',  $file1,  "1st line\n" ],
      [ 'file2',  $file2,  "2nd line\n1st line\n" ],
      [ 'file3',  $file3,  "3rd line\n2nd line\n1st line\n" ],
      [ 'file21', $file21,
        "\n\n\n\n\n\n\n\n\n12th line\n11th line\n10th line\n".
        "9th line\n8th line\n7th line\n6th line\n5th line\n".
        "4th line\n3rd line\n2nd line\n1st line\n" ],
   );
   for my $r (@tt) { CheckOne($scr, $r->[0], $r->[1], $r->[2]) }
}

sub CheckMid {
   my ($scr) = @_;
   my @tt = (
      [ 'file1',  $file1,  "1st line\n" ],
      [ 'file2',  $file2,  "1st line\n2nd line\n" ],
      [ 'file3',  $file3,  "2nd line\n" ],
      [ 'file4',  $file4,  "2nd line\n3rd line\n" ],
      [ 'file12', $file12, "6th line\n7th line\n" ],
      [ 'file21', $file21, "11th line\n" ],
   );
   for my $r (@tt) { CheckOne($scr, $r->[0], $r->[1], $r->[2]) }
}

sub CheckWc {
   my ($scr) = @_;
   my @tt = (
      [ 'file1',  $file1,  "0000000001\n" ],
      [ 'file2',  $file2,  "0000000002\n" ],
      [ 'file3',  $file3,  "0000000003\n" ],
      [ 'file4',  $file4,  "0000000004\n" ],
      [ 'file12', $file12, "0000000012\n" ],
      [ 'file21', $file21, "0000000021\n" ],
   );
   for my $r (@tt) { CheckOne($scr, $r->[0], $r->[1], $r->[2]) }
}

# -----------------------------------------------------

my $head = 'head.p6';
my $tail = 'tail.p6';
my $rev  = 'rev.p6';
my $mid  = 'mid.p6';
my $wc   = 'wc.p6';
select(STDERR);$|=1;select(STDOUT);$|=1;  # auto-flush
-f $head or die "error: file '$head' not found.\n";
-f $tail or die "error: file '$tail' not found.\n";
-f $rev  or die "error: file '$rev' not found.\n";
-f $mid  or die "error: file '$mid' not found.\n";
-f $wc   or die "error: file '$wc' not found.\n";
PrintGolfScore($head, $tail, $rev, $mid, $wc);
CheckHead($head);
CheckTail($tail);
CheckRev($rev);
CheckMid($mid);
CheckWc($wc);
PrintGolfScore($head, $tail, $rev, $mid, $wc);
print "Hooray, you passed.\n";
