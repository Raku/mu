#!/usr/bin/pugs
# Animal guessing game, based on http://www.perlmonks.org/?node_id=10368

use v6;

sub try(Any $this) {
  # XXX $this ~~ Hash'd be nicer
  # say "c {$this.perl}";
  # say "d {$this.ref}";
  if(ref $this eq "Hash") {
    my $yesno    = yes($this<question>) ?? "yes" !! "no";
    my %new      = $this;
    # say "a {%new.ref}";
    # say "a' {(%new{$yesno}).ref}";
    %new{$yesno} = try %new{$yesno};
    # say "b {%new.ref}";
    # say "b' {(%new{$yesno}).ref}";
    return \%new;
  }

  if (yes "Is it a $this") {
    say "I got it!";
    return $this;
  }

  print "No!?  What was it then? ";
  # XXX: chomp(my $new = =$*IN)'d be nicer
  my $new = =$*IN; $new .= chomp;
  print "And a question that distinguishes a $this from a $new would be? ";
  my $q   = =$*IN; $q .= chomp;
  my $yes = yes "And for a $new, the answer would be...";

  # XXX Pugs doesn't handle hashref construction by {...} correctly currently,
  # so this ugly hack is needed.
  my %new = (
    question => $q,
    yes      => sub { $yes ?? $new  !! $this }.(),
    no       => sub { $yes ?? $this !! $new  }.(),
  );
  # say "f {%new.perl}";
  # say "g {(\%new).ref}";
  # say "h {\%new.ref}";
  return \%new;
}

sub yes(Str $q) {
  print "$q (yes/no)? ";

  my $input = lc substr(=$*IN, 0, 1);
  
  return
    $input eq "y" ?? 1 !!
    $input eq "n" ?? 0 !! yes($q);
}

my $info = "dog";

while(1) {
  $info = try $info;
  last() unless yes "Play again?";
}

say "Bye!";
say $info.perl;
