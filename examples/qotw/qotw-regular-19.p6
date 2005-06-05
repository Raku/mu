#!/usr/bin/pugs
# Perl 6 solution to QoTW regular #19, see
# http://perl.plover.com/qotw/r/solution/019.
# Note: Currently doesn't work in Pugs.

my ($file, $scale) = @*ARGS;
my %sails;

die "No such file: \"$file\"" unless $file and -e $file;
$scale //= 100;

my $FH = open $file err die "Could't open \"$file\"!\n";

for =$FH -> $line {
  next() if $line ~~ rx:perl5/^#/;
  next() if $line ~~ rx:perl5/^\s*$/;

  my ($sail_info, $coord)    = split ":", $line;
  my ($sail_name, $verticie) = split ".", $sail_info;
  my ($x, $y)                = split ",", $coord;
  $x ~~ s:perl5:g/\s*//;
  $y ~~ s:perl5:g/\s*//;

  push %sails{$sail_name}<points>, [$x, $y];
}

my $total = 0;
for %sails.keys -> $sail {
  my $sail_area = _calc_area($sails{$sail}<points>) * ($scale / 100);
  say "$sail.area: $sail_area cm^2";
  $total += $sail_area;
}

say "total.area: $total cm^2";

close $FH;

sub calc_area($p) {
  my ($x, $y) = conv_p_to_x_y($p);
  my $area;

  # from http://mathworld.wolfram.com/PolygonArea.html
  my $i;
  loop($i = 0; $i < +$x - 1; $i++) {
    $area += ($x[$i] * $y[$i+1] - $x[$i+1] * $y[$i]);
  }

  my $last_x = pop $x;
  my $last_y = pop $y;

  $area += ($last_x * $y[0] - $x[0] * $last_y);

  return abs($area) / 2;
}

sub _conv_p_to_x_y($p) {
  my $x;
  my $y;
  
  for $p -> $x_y {
    push $x, $x_y[0];
    push $y, $x_y[1];
  }
  
  return ($x, $y);
}
