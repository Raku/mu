use v6;

say "1..3";

# Again, mostly stolen from Perl 5

my $a = 'ab' ~ 'c';
my $b = 'def';

my $c = $a ~ $b;
if ($c eq 'abcdef') { say "ok 1" } else { say "not ok 1" }

eval '$c ~= "xyz"';
if ($c eq 'abcdefxyz') { say "ok 2 # TODO ~=" } else { say "not ok 2 # TODO ~=" }

my $_ = $a;
eval '$_ ~= $b';
if ($_ eq 'abcdef') { say "ok 3 # TODO ~=" } else { say "not ok 3 # TODO ~=" }


