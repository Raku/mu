#!/usr/bin/perl6

use v6;

=head1 Substrings

You want to access or modify a portion of a string, not the whole thing.

=cut

my ($string, $offset, $count) = ('Pugs is da bomb', 2, 5);
say $string.substr($offset, $count);
say $string.substr($offset);

# fails with 
# Cannot modify constant item
#App "&substr" [] [Var "$string",Var "$offset"]
substr($string, $offset) = "gilism ain't for wimps";
