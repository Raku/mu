#!/usr/bin/pugs

use v6;
use Test;

# See thread "Demagicalizing pair" on p6l started by Luke Palmer,
# http://article.gmane.org/gmane.comp.lang.perl.perl6.language/4778/ and
# http://colabti.de/irclogger/irclogger_log/perl6?date=2005-10-09,Sun&sel=528#l830.

# To summarize:
#   foo(a => 42);  # named
#   foo(:a(42));   # named
#
#   foo((a => 42));  # pair passed positionally
#   foo((:a(42)));   # pair passed positionally
#
#   my $pair = (a => 42);
#   foo($pair);      # pair passed positionally
#   foo(*$pair);     # named

plan 16;

{
    my sub foo ($a, $b) { "[$a] [$b]" }

    is foo(a => 42, 23), "[42] [23]", "'a => 42' is a named";
    is foo(:a(42),  23), "[42] [23]", "':a(42)' is a named";

    is foo((a => 42), 23), "[a\t42] [23]", "'(a => 42)' is a pair", :todo<unspecced>;
    is foo((:a(42)),  23), "[a\t42] [23]", "'(:a(42))' is a pair",  :todo<unspecced>;
}

{
    my sub foo (+$a) { "[$a]" }

    is foo(a => 42), "[42]", "'a => 42' is a named";
    is foo(:a(42)),  "[42]", "':a(42)' is a named";

    dies_ok { foo((a => 42)) }, "'(a => 42)' is a pair", :todo<unspecced>;
    dies_ok { foo((:a(42)))  }, "'(:a(42))' is a pair",  :todo<unspecced>;
}

{
    my sub foo ($a) { "[$a]" }

    my $pair = (a => 42);

    is foo($pair),  "[a\t42]", 'a $pair is not treated magically...', :todo<feature>;
    is foo(*$pair), "[42]",    '...but *$pair is', :todo<feature>;
}

{
    my sub foo ($a)    { "[$a]" }
    my sub get_pair () { (a => 42) }

    is foo(get_pair()),  "[a\t42]", 'get_pair() is not treated magically...', :todo<feature>;
    is foo(*get_pair()), "[42]",    '...but *get_pair() is', :todo<feature>;
}

{
    my sub foo ($a) { "[$a]" }

    my @array_of_pairs = (a => 42);

    is foo(@array_of_pairs), "[a\t42]",
        'an array of pairs is not treated magically...', :todo<feature>;
    is foo(*@array_of_pairs), "[42]",
        '...but *@array is', :todo<unspecced>;
}

{
    my sub foo ($a) { "[$a.ref()]" }

    my %hash_of_pairs = (a => "str");

    is foo(%hash_of_pairs),  "[Hash]", 'a hash is not treated magically...';
    is foo(*%hash_of_pairs), "[Str]",  '...but *%hash is', :todo<unspecced>;
}
