use v6;

=pod
Arrays

=cut

say "1..5";

my @arry = ("foo", "bar", "ok 1" , "ok 3");

say  @arry[2];

if (@arry[1] eq "bar") {say "ok 2" } else { say "not ok 2" }

@arry[3].say;

if ( @arry == 4) { print "ok 4" } else { say "not ok 4" }
if (+@arry == 4) { print "ok 5" } else { say "not ok 5" }
