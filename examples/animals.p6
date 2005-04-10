#!/usr/bin/pugs
# Animal guessing game, based on http://www.perlmonks.org/?node_id=10368

sub try(Any $this is rw) {
  # XXX $this ~~ Hash'd be nicer
  say $this.perl;
  say $this.ref;
  if(ref $this eq "Hash") {
    # XXX: $subthis is needed to work around a Pugs bug ("modifying a constant
    # item").
    my $yesno   = yes($this<question>) ?? "yes" :: "no";
    # XXX "cannot cast as Str: MVal <mval>"
    my $subthis = $this{$yesno};
    try $subthis;
    $this{$yesno} = $subthis;
    return $this;
  }

  if (yes "Is it a $this") {
    say "I got it!";
    return 1;
  }

  print "No!?  What was it then? ";
  # XXX: chomp(my $new = =$*IN)'d be nicer
  my $new = =$*IN; chomp $new;
  print "And a question that distinguishes a $this from a $new would be? ";
  my $q   = =$*IN; chomp $q;
  my $yes = yes "And for a $new, the answer would be...";

  # XXX Pugs doesn't handle hashref construction by {...} correctly currently,
  # so this ugly hack is needed.
  my %new = (
    question => $q,
    yes      => sub { $yes ?? $new  :: $this }.(),
    no       => sub { $yes ?? $this :: $new  }.(),
  );
  $this = %new;

  return 0;
}

sub yes(Str $q) {
  print "$q (yes/no)? ";

  my $input = lc substr(=$*IN, 0, 1);
  
  return
    $input eq "y" ?? 1 ::
    $input eq "n" ?? 0 :: yes($q);
}

my $info = "dog";

while(1) {
  try($info);
  last() unless yes "Play again?";
}

say "Bye!";
say $info.perl;
