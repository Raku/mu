use v6-alpha;

# Please remember to update t/examples/examples.t and rename
# examples/output/junctions/1 if you rename/move this file.

my @color = <red green blue>;

my $x = any @color;

my $y = 'blue';

my $result = ($x eq $y) ?? "acceptable" !! 'unacceptable' ;

print "$result color\n";

  
