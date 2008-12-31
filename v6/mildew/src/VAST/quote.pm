package VAST::quote;
use utf8;
use strict;
use warnings;
use AST::Helpers;

sub emit_m0ld {
    my $m = shift;
    # XXX escapes
    string join '',map {ref $_ ? $_->as_constant_string : $_} @{$m->{nibble}{nibbles}};
}

1;
