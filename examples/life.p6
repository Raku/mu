#!/usr/bin/pugs
use v6;

# life.p6 adopted for perl6 after:
#
# // life.cola
# //
# // Game of life
# //
# // Copyright (C) 2002 Melvin Smith
# //
#
# (c) 2002 by Leopold Toetsch

# Input / output are int arrays - slooow - needs a rewrite
#
sub print_world($world) {  
    for ($world) -> $row {
          say $row.map:{ +$_ ?? '*' !! ' '}.join("");
    }
    say "----------------";
}

sub neighbors($cell_x, $cell_y, $input) {
   my $neighbors;
   for -1,0,1 -> $x_off {
      for -1,0,1 -> $y_off {       
       $neighbors +=  $input[$cell_x + $x_off][$cell_y + $y_off];
     }
   }
   return $neighbors;
}

sub sycle($input) {

    my @death = (0,0,1,1,0,0,0,0,0);
    my $output;
    for 0..15 -> $x {
      print ".";
      for 0 .. 15 -> $y {
         my $neighbors = neighbors($x,$y,$input);        
          if ($input[$x][$y]) {
                  if (@death[$neighbors]) {
                      $output[$x][$y] = 1;
                      
                  }
                  else {
                      $output[$x][$y] = 0;
                  }
          } else {
            if ($neighbors == 3) {
                $output[$x][$y] = 1;
            }
         }
      }
    }
    say "";
    return $output;
}

my  $world = (
    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,],
    [0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,],
    [0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,],
    [1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,],
    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,],
    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,],
    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,],
    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,],
    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,],
    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,],
    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,],
    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,],
    [0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,],
    [0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,],
    [0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,],
    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
);

my $gen = @*ARGS[0] || 100;
say "Running ", $gen, " generations";
my $ts = time;

for 1 .. $gen {
  print_world($world);
  $world = sycle($world);
}
my $tdelta = time() - $ts + 1;

my $ratio = $gen / $tdelta;
say "Gens/s: ", $ratio;
