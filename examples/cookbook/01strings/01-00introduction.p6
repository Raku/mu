#!/usr/bin/perl6

use v6;

=head1 NAME

Perl 6 Cookbook: Introduction to Strings

=head1 Description

=head1 Discussion

A string can be created implicitly, by declaring a scalar and assigning a 
quoted value to it:

	# declare a scalar and place a string into it
	my $scalar = 'This is a scalar holding a String';

Or it can be declared explicitly:

	my Str $string = 'This Str is holding a String';

Assignments of non-strings can cause an implicit conversion:

	$string = 1234;               # Implicit conversion

TODO: a short paragraph on the difference between the my $scalar 
and my Str $string examples above.

=head2 Variable Interpolation

The simplest way to interpolate variables within a literal string is to use
double quotes around the value:

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

XXX Some backslashy escapes work in single quotes too

=head2 Using Alternative Delimiters

It's often useful to use something other than single or double quotes when
declaring strings. To do so use the q// and qq// quote operators:

XXX I believe the "standard" form is now with square brackets, i.e. q[],
instead of q//.
	-- I see the q// form most often in synopsis, can you confirm a change to
		q[] somewhere? --gcomnz
XXX No - must have dreamt it... (Remove this paragraph when you've read it.)

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

	# Interpolate @ vars only:
	say q:a/The quick brown @animal[1] jumps over the lazy @animal[2]/;
	say qa/The quick brown @animal[1] jumps over the lazy @animal[2]/;
	say qa/We have @animal.elems() elements in the \@animals array/;

XXX But @animal[1] is not an array, it is an array element.
	-- see comment below
	
	# interpolate % vars only:
	say q:h/The quick brown %animal{'quick'} jumps over the.../;
	say qh/The quick brown %animal{'quick'} jumps over the.../;

XXX But %animal{quick} is not a hash, it is a hash element.
	-- i think that's irrelevant to the examples, however I will also 
		demonstrate the many other things you can do with hashes and 
		arrays during interpolation --gcomnz
XXX It's relevant because what you say now is just wrong. Both arrays|hashes
*and* array|hash elements are interpolated. While %foo is a hash, %foo{bar} is
just a scalar! 
	-- yeah, you're right. i went looking for a short way to say
		what i mean, and it seems like the synopses is the closest
		i can get right now, so i replaced the comments with
		"% vars" and "@ vars" for now. I'm hesitant to put in an
		interpolation of actual straight %hash and @array right now
		because it seems like it's too complex an example for
		chapter 01-00 --gcomnz
XXX By the way, you probably want %animal{'quick'} there, because
%animal{quick} is %animal{quick()}.
	-- phew, thanks, I just caught up on that whole hash
		subscripting thread  --gcomnz

	# interpolate functions only: both & and () are required
	say q:f/The quick brown &get_animal('quick') jumps.../;
	say qf/The quick brown &get_animal('quick') jumps.../;

	# interpolate closures only:
	say q:c/The quick brown { return 'fox'; } jumps.../;
	say qc/The quick brown { return 'fox'; } jumps.../;

	# interpolate backslash codes only:
	say q:b/The quick brown fox\n\tJumps over the lazy dog/;
	say qb/The quick brown fox\n\tJumps over the lazy dog/;

Adverbs can be strung together to make a specialized quoting environment for
your string.

	# interpolate only scalars and arrays:
	say q:s:a/The quick brown $fox jumps over the lazy @animal[2]/;

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
removed up to the same amount of indentation used by the closing delimiter, 
a tab character being equal to 8 normal spaces.

A here document can be as exacting with adverbs as any other quoted string. For
instance you can specify that you only want scalars interpolated by adding
the :s adverb.

	# This multiline string will only interpolate scalars
	my $multiline = q:s:to/EOF/
		This $scalar will be interpolated, but this @array won't be.
                EOF

These adverbs apply to the body of the heredoc, not to the terminator, because
the terminator has to be known at compile time. This means that
q:s:to/EO$thing/ doesn't do what you mean.

=cut
