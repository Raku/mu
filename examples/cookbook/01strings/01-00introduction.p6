#!/usr/bin/perl6

use v6;

=head1 NAME

Perl 6 Cookbook: Introduction to Strings

=head1 Description

=head2 Explicit and Implicit Strings

A string can be declared implicitly, by defining an scalar and assigning a 
quoted value to it:

	my $scalar = 'This is scalar is holding a String';

Or it can be declared explicitly:

	my Str $string = 'This is scalar is holding a String';

Explicit assignment leads to an error if the programmer attempts to assign
a non-String to the scalar:

	my Str $string;
	$string = 1234;               # Error
	$string = { print "block"; }; # Error
	$string = 'String';           # Okay

=head2 Variable Interpolation

The simplest way to interpolate variables within a string is to use double
quotes in the definition:

	my ($var1, $var2) = <dog fox>;
	say "The quick brown $var1 jumps over the lazy $var2";

Double quoted strings can also interpolate arrays, hashes, backslashed control
characters, and other good stuff:

	# interpolate arrays:
	my @animal = <fox dog>;
	say "The quick brown @animal[1] jumps over the lazy @animal[2]";
	
	# interpolate hashes:
	my %animal = (quick => 'fox', lazy => 'dog');
	say "The quick brown %animal{quick} jumps over the lazy...";

	# interpolate special backslash values:
	print "The quick brown fox\n\tjumps over the lazy dog\n";

=head2 Using Alternative Delimiters

It's often useful to use something other than single or double quotes when
declaring strings. To do so use the q// and qq// quote operators:

	# Single quoted strings
	say 'I have to escape my \'single quotes\' in this string';
	say q/This string allows 'single quotes' seamlessly/;

	# Double quoted strings
	say "I have to escape my \"double quotes\" in this string";
	say q/This string allows "double quotes" seamlessly/;

The slashes in q// and qq// can be replaced with most other delimiters.

	# Single quoted strings
	say q'Many delimiters are available for quoting';
	say q"Many delimiters are available for quoting";
	say q`Many delimiters are available for quoting`;
	say q[Many delimiters are available for quoting];
	say q(Many delimiters are available for quoting);
	say q<Many delimiters are available for quoting>;
	say q{Many delimiters are available for quoting};
	say q«Many delimiters are available for quoting»;

=head2 Advanced Interpolation Control

Perl 6 allows very fine control over string quoting using the q// quote
operator with specialized adverbs. For instance, q:s// signifies that we only
want scalars interpolated. These adverbs can also be expressed in a short form,
for instance q:s// can be expressed as qs//.

	# Raw quoting: no escaping at all (unless otherwise adverbed)
	say q:0/Here is a code sample &function($param) (no interpolation)/;
	say q0/Here is a code sample &function($param) (no interpolation)/;

	# Single quoting:
	say 'Lots of options for single quotes';
	say q/Lots of options for single quotes/;
	say q:1/Lots of options for single quotes/;
	say q1/Lots of options for single quotes/;
	
	# Double quoting: interpolates scalars, arrays, hashes, functions,
	# closures, and backslash codes
	say "Plenty of ways to double quote too";
	say qq/Plenty of ways to double quote too/;
	say q:2/Plenty of ways to double quote too/;
	say q2/Plenty of ways to double quote too/;

	# Interplate scalars only:
	my ($var1, $var2) = <dog fox>;
	say q:s/The quick brown $var1 jumps over the lazy $var2/;
	say qs/The quick brown $var1 jumps over the lazy $var2/;

	# Interpolate arrays only:
	say q:a/The quick brown @animal[1] jumps over the lazy @animal[2]/;
	say qa/The quick brown @animal[1] jumps over the lazy @animal[2]/;
	
	# interpolate hashes only:
	say q:h/The quick brown %animal{quick} jumps over the.../;
	say qh/The quick brown %animal{quick} jumps over the.../;

	# interpolate functions only: both & and () are required
	say q:f/The quick brown &get_animal('quick') jumps.../;
	say qf/The quick brown &get_animal('quick') jumps.../;

	# interpolate closures only:
	say q:c/The quick brown { return 'fox'; } jumps.../;
	say qc/The quick brown { return 'fox'; } jumps.../;

	# interpolate backslash codes only:
	say q:b/The quick brown fox\n\tJumps over the lazy dog/;
	say qb/The quick brown fox\n\tJumps over the lazy dog/;

=head2 Defining Multiline Strings (Here Documents)

Multiline strings (here documents) can be defined using the q// and qq//
operators with the :to adverb added.

	# A double quoted multiline string:
	my $a = qq:to/EOF/
		This is a multiline here document terminated by EOF on a 
		line by itself with any amount of whitespace before or 
		after the termination string. Leading whitespace equivalent 
		to the indentation of the delimiter will be removed from 
		all preceding lines.
		EOF

When defined in this way the whitespace at the start of each line will be 
removed up to the same amount of indentation used by the closing delimiter.


# XXX - question: How equal are bunches of spaces to tabs?
# 	-- I'd say that's a question for perl6lang

=cut
