
use v6-**;
say "1..1";

my $has_run = 0;

eval '

=begin

1 + 3
something

=end

$has_run = 1;

';


if $has_run {
    say "ok 1";
}
else {
    say "not ok 1 # TODO";
}

