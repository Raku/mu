use v6-alpha;

use Test;

plan 8;

=pod

Testing regex modifiers.  There are a bunch of these, and I have only
just started the list.  Please fill in more, preferably in the order
they are listed in the synopsis, and with accurate links.

=cut

if !eval('("a" ~~ /a/)') {
  skip_rest "skipped tests - rules support appears to be missing";
  exit;
}

#L<S05/Modifiers/"The :i">

{
  regex mixedcase { Hello };
  "Hello" ~~ m/<mixedcase>/;
  is(~$/, "Hello", "match mixed case");
  "hello" ~~ m/<mixedcase>/;
  is(~$/, "", "do not match lowercase");
  "hello" ~~ m:i/<mixedcase>/;
  is(~$/, "hello", "match with :i");
  "hello" ~~ m:ignorecase/<mixedcase>/;
  is(~$/, "hello", "match with :ignorecase");
};

#L<S05/Modifiers/"The :c">

{
  regex simple { . a };
  my $string = "1a2a3a";
  $string ~~ m:c/<simple>/;
  is(~$/, '1a', "match first 'a'");
  $string ~~ m:c/<simple>/;
  is(~$/, '2a', "match second 'a'");
  $string ~~ m:c/<simple>/;
  is(~$/, '3a', "match third 'a'");
  $string ~~ m:c/<simple>/;
  is(~$/, '', "no more 'a's to match");
};

