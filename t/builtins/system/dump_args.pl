($out,$") = splice @ARGV, 0, 2;
$" = chr($");

open STDOUT, ">$out"
  or die "Couldn't create '$out' : $!";
binmode STDOUT;
print "@ARGV";