#!/usr/bin/pugs
# Animal guessing game, based on http://www.perlmonks.org/?node_id=10368

use v6;

sub try(Any $this) {
  if ($this ~~ Hash) {
    my $yesno    = yes($this<question>) ?? "yes" !! "no";
    my %new      = $this;
    %new{$yesno} = try %new{$yesno};
    return \%new;
  }

  if (yes "Is it a $this") {
    say "I got it!";
    return $this;
  }

  print "No!?  What was it then? ";
  my $new = =$*IN;
  print "And a question that distinguishes a $this from a $new would be? ";
  my $q   = =$*IN;
  my $yes = yes "And for a $new, the answer would be...";

  my %new = (
    question => $q,
    yes      => sub { $yes ?? $new  !! $this }.(),
    no       => sub { $yes ?? $this !! $new  }.(),
  );
  return \%new;
}

sub yes(Str $q) {
  print "$q (yes/no)? ";

  my $input = lc substr(=$*IN, 0, 1);
  given $input {
        when "y" { 1 }
        when "n" { 0 }
        default  { yes($q) }
  }
}

my $info = "dog";

while(1) {
  $info = try $info;
  last unless yes "Play again?";
}

say "Bye!";
say $info.perl;
