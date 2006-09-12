# This script needs a comment explaining it's purpose in the test suite
# or may be moved or deleted. It doesn't seem to be used by the other
# "*.t" files in this directory. 

($out,$") = splice @ARGV, 0, 2;
$" = chr($");

open STDOUT, ">$out"
  or die "Couldn't create '$out' : $!";
binmode STDOUT;
print "@ARGV";
