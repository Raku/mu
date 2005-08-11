my @new_data   = qw(100 90 70);
my @old_data   = qw(55 35 5);

my $epsilon = 50;

if ( $epsilon > (all(@new_data) - any(@old_data))  ) {
  say "data is close enough. add it."
} else {
  say "new data is not close enough. DO NOT ADD";
}

for @old_data -> $old_datum {
  print "[ $old_datum ]\t";
  for @new_data -> $new_datum {
    print $new_datum - $old_datum;
    print "\t";
  }
  print "\n";
}      
