#!/usr/bin/perl6

use v6;

=head1 Trimming blanks from a string

=cut

# XXX What are "blanks"? How many kinds of unicode whitespace are matched by
# this thing? What about non-breaking whitespace? And maybe make clear that
# we're only doing something about leading and trailing "blanks".

# To trim a string
# (Basing this on discussion at http://tinyurl.com/4xjnh)
$string = $string.trim;
$string = trim(string);

# To remove the last character from a string:
chop($string);
$string.chop;
# XXX Maybe note that this is just short for a substr call?
