#!/usr/bin/pugs
# Perl 6 solution to QoTW regular #24, see
# http://perl.plover.com/qotw/r/solution/024.

my $transition_file = @*ARGS[0] err
  die "Usage: $*PROGRAM_NAME transition_file [initial_tape]\n";

my $initial_tape = @*ARGS[1] // "_";

my $i    = 0;
my %tape = map { $i++, $^a }, split "", $initial_tape;

my %instructions;

my $state;
my $tape_loc = 0;

my $trans = open "< $transition_file" err
  die "Can't open \"$transition_file\" for reading!\n";

for =$trans {
  my $line = $_; # avoid "can't modify a constant"
  $line ~~ s:perl5/#.*//;

  if($line ~~ rx:perl5/\S/) {
    $line ~~ rx:perl5/^\s*(\w+)\s+(\w)\s+(\w+)\s+(\w)\s+([LR])\s*$/;
    my ($current_state, $current_char, $new_state, $new_char, $direction) =
      ($1, $2, $3, $4, $5);

    if(%instructions{"$current_state $current_char"}) {
      die "$current_state $current_char redefined.\n";
    }

    $state //= $current_state;

    %instructions{"$current_state $current_char"} =
      "$new_state $new_char $direction";
  }
}

my $x = "$state %tape{$tape_loc}";
my $instruction;
while($instruction = %instructions{"$state %tape{$tape_loc}"}) {
  my ($new_state, $new_char, $direction) = split " ", $instruction;

  $state = $new_state;
  %tape{$tape_loc} = $new_char;

  if($direction eq 'L') {
    $tape_loc--;
  } else {
    $tape_loc++;
  }

  if(not %tape.exists($tape_loc)) {
    %tape{$tape_loc} = '_';
  }
}

my $final_tape = join "", %tape{sort {$^a <=> $^b} keys %tape};
$final_tape ~~ s:Perl5/^_+//;
$final_tape ~~ s:Perl5/_+$//;

say $final_tape;
