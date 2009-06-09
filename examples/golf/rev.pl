my @l=@*ARGS[0].open.lines;
my $x;map {$x=$_~$x},@l;
print $x;
