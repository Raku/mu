package Smart::Links;
use strict;
use warnings;
use 5.006;

our $VERSION = '0.01';

use base 'Exporter';
our @EXPORT = qw(parse_pattern process_paragraph);

=head1 NAME

Smart::Links - connecting test files with pod documentation

=head1 SYNOPTIS

 smartlinks.pl
 
=head1 DESCRIPTION

=cut



# convert patterns used in 00-smartlinks.to perl 5 regexes
sub parse_pattern ($) {
    my $pat = shift;
    my @keys;
    while (1) {
        if ($pat =~ /\G\s*"([^"]+)"/gc ||
            $pat =~ /\G\s*'([^']+)'/gc ||
            $pat =~ /\G\s*(\S+)/gc) {
                push @keys, $1;
        } else { last }
    }
    my $str = join('.+?', map {
        my $key = quotemeta $_;
        $key =~ s/^\w/\\b$&/;
        $key =~ s/\w$/$&\\b/;
        $key;
    } @keys);

    $str;
}

# process paragraphs of the synopses: unwrap lines, strip POD tags, and etc.
sub process_paragraph ($) {
    my $str = shift;

    # unwrap lines:
    $str =~ s/\s*\n\s*/ /g;

    # strip POD tags:
    # FIXME: obviously we need a better way to do this:
    $str =~ s/[LCFIB]<<<\s+(.*?)\s+>>>/$1/g;
    $str =~ s/[LCFIB]<<\s+(.*?)\s+>>/$1/g;
    $str =~ s/[LCFIB]<(.*?)>/$1/g;
    $str;
}



=head1 AUTHOR

Agent Zhang (E<lt>agentzh@gmail.comE<gt>) wrote the initial
implementation, getting help from many others in the Pugs team.

Current maintainer: The Pugs team

=head1 COPYRIGHT

Copyright (c) 2006 - 2009 by the Pugs Team.

=head1 LICENSE

Smart::Links is free software; you can redistribute it and/or modify it under the
terms of the Artistic License 2.0.  (Note that, unlike the Artistic License
1.0, version 2.0 is GPL compatible by itself, hence there is no benefit to
having an Artistic 2.0 / GPL disjunction.)

=cut


1;
