use v6;

=pod
Arrays

=cut

say "1..3";

my @arry = ("foo", "bar", "ok 1" , "ok 2");

say  @arry[2];

if (@arry[1] eq "bar") {say "ok 2" } else { say "not ok 2" }

@arry[3].say;
