#!/usr/bin/pugs

# Expert QOTW #7
# http://perl.plover.com/qotw/e/solution/007

use v6;

my $EMPTY = ' ';
my $VAPOR = '.';
my $ICE = '*';

my $t = 0;

frost(create_volume(@ARGS[0],@ARGS[1],@ARGS[2]));

sub cls returns Void {
    system(($?OS eq any<MSWin32 mingw cygwin>) ?? 'cls' !! 'clear');
}

# create a volume of size M x N with vapor density D
# and a single ice particle in the center
sub create_volume (?$m is copy,?$n is copy,?$d is copy) {
  $m //= 10; $n //= 20; $d //= 20;
  die "\$m must be even\n" if $m % 2;
  die "\$n must be even\n" if $n % 2;

  my @vol;
  for 1..$m -> $x {
    for 1..$n -> $y {
      @vol[$x][$y] = rand(100) < $d ?? $VAPOR !! $EMPTY;
    }
  }
  @vol[$m/2][$n/2] = $ICE;

  return @vol;
}

# run frost simulation until all vapor freezes,
# displaying volume each cycle.
sub frost (@vol is copy) {
  my ($m, $n) = (+@vol, +@vol[1]);

  cls; say join "", @$_ for @vol;
  say "t = $t";
  while (cycle($t++, $m, $n, @vol)) {
  cls; say join "", @$_ for @vol;
    say "t = $t";
  }
  cls; say join "", @$_ for @vol;
  say "Finished at t = $t";
}


# run a single cycle of frost simulation
sub cycle ($t, $m, $n, @data is rw) {
  my ($x,$y,$v);

  if $t % 2 {
    # boundary conditions -- do these seperately to keep
    # the checks out of the main loop
    # Corner
    $v = process(@data[-1][-1], @data[-1][0],
                 @data[ 0][-1], @data[ 0][0]);
    # vertical seam
    loop ($y = 1; $y < $n - 2; $y += 2) {
      $v = process(@data[-1][$y], @data[-1][$y+1],
                   @data[ 0][$y], @data[ 0][$y+1]) || $v;
    }
    # horizontal seam
    loop ($x = 1; $x < $m - 2; $x += 2) {
      $v = process(@data[$x][-1], @data[$x+1][-1], @data[$x][ 0], @data[$x+1][ 0]) || $v;
    }
    # center
    loop ($x = 1; $x < $m - 2; $x += 2) {
      loop ($y = 1; $y < $n - 2; $y += 2) {
        $v = process(@data[ $x ][$y], @data[ $x ][$y+1],
                     @data[$x+1][$y], @data[$x+1][$y+1]) || $v;
      }
    }

  } else {
    loop ($x = 0; $x < $m; $x += 2) {
      loop ($y = 0; $y < $n; $y += 2) {
        $v = process(@data[ $x ][$y], @data[ $x ][$y+1],
                     @data[$x+1][$y], @data[$x+1][$y+1]) || $v;
      }
    }
  }
  return $v;
}

# process a neigborhood
sub process ($a is rw,$b is rw,$c is rw,$d is rw) {
  my ($vapor, $ice);

  for $a,$b,$c,$d {
    $vapor++ if $_ eq $VAPOR;
    $ice++   if $_ eq $ICE;
  }

  # short-circuit if nothing to do.
  # faster, but it makes the output jumpy on my system.
  # things may look smoother without it.
  return 0 unless $vapor;

  if ($ice) {
    # freeze vapor
    ($a,$b,$c,$d) = map {$_ eq $VAPOR ?? $ICE !! $_} ($a,$b,$c,$d);
  }
  else {
    # randomly rotate vapor
    if (int rand(2)) {
      ($a,$b,$c,$d) = ($b,$d,$a,$c);
    }
    else {
      ($a,$b,$c,$d) = ($c,$a,$d,$b);
    }
  }
  return $ice ?? 0 !! $vapor;
}
