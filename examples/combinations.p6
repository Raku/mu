sub combinations returns Array  (@list is rw) {
  return () unless @list.elems;
  my @ans;
  for 1 .. 2**@list.elems-1-> $num {
      push @ans, [ @list[ (0 .. sqrt($num)).grep:{ $num +& (2**$_) } ] ];
  }
  return @ans;
} 
my @list = (1..4);
combinations(@list).perl.say;
