#!/usr/bin/perl6

use v6;

=head1 Trimming blanks from a string

=cut

# To trim a string
# (Basing this on discussion at http://tinyurl.com/4xjnh)
$string = $string.trim;
$string = trim(string);

# To remove the last character from a string:
chop($string);
