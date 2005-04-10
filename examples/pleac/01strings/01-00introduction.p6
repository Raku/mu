#!/usr/bin/perl6

use v6;

=head1 Introduction

You want to declare a string.

=cut

# Single interpolated quote strings
# Interpolate \\, \q and \' (or whatever)
$string = '\n';                     # two characters, \ and an n
$string = 'Jon \'Maddog\' Orwant';  # literal single quotes

# Generalized single quoted forms
$string = q/Jon 'Maddog' Orwant/;   # literal single quotes
$string = q1/Jon 'Maddog' Orwant/;  # explicit adverb in shortened form
$string = q:1/Jon 'Maddog' Orwant/; # full form of single quote syntax

# Alternative operators can be used in the place of // delimeters
$string = q[Jon 'Maddog' Orwant];   # literal single quotes
$string = q{Jon 'Maddog' Orwant};   # literal single quotes
$string = q(Jon 'Maddog' Orwant);   # literal single quotes
$string = q<<Jon 'Maddog' Orwant>>; # literal single quotes
$string = q«Jon 'Maddog' Orwant»;   # literal single quotes
$string = q'Jon 'Maddog' Orwant';   # literal single quotes
$string = q"Jon 'Maddog' Orwant";   # literal single quotes
$string = q`Jon 'Maddog' Orwant`;   # literal single quotes

# Double interpolated quote strings
# Interpolate all the following vars: $ vars, @ vars, % vars, & calls,
# {...} expressions, \t, \n, etc
$string = "\n";                     # a "newline" character
$string = "Jon \"Maddog\" Orwant";  # literal double quotes

# Generalized double quoted forms
$string = qq/Jon "$nick" Orwant/;   # literal single quotes
$string = q2/Jon "$nick" Orwant/;   # explicit adverb in shortened form
$string = q:2/Jon "$nick" Orwant/;  # full form of double quote syntax

# Alternative operators can be used in the place of // delimeters
$string = q[Jon 'Maddog' Orwant];   # literal single quotes
$string = q{Jon 'Maddog' Orwant};   # literal single quotes
# etc...

# Multiline strings follow the same generalized q// forms with the addition
# of a :to adverb and a termination string
$a = qq:to/EOF/
	This is a multiline here document terminated by EOF on a line by itself
	with any amount of whitespace before or after the termination string.
	Leading whitespace equivalent to the indentation of the delimiter will 
	be removed from all preceding lines.
	EOF

