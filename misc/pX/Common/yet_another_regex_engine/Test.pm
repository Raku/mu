package Test;
require Exporter;
@ISA=(Exporter);
@EXPORT=@EXPORT_OK=qw(plan skip_rest force_todo ok is);

my $count = 0;
my $count_to = undef;
sub plan ($) {
  my($n)=@_;
  $count_to = $n;
  print "\n1..$n\n";
}
sub skip_rest ($) {
  my($msg)=@_;
  for(($count+1)..$count_to){print "ok # skip  $msg\n";}
}
sub force_todo {
}
sub ok {
  my($b,$msg)=@_;
  print $b ? "ok\n" : "not ok # $msg\n";
}
sub is {
  my($v1,$v2,$msg)=@_;
  print $v1."" eq $v2."" ? "ok\n" : "not ok # $msg\n";
}

1;
__END__
