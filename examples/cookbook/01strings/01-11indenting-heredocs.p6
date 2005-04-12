#!/usr/bin/perl6

use v6;

=head1 Indenting Here Documents

=cut

# indent a here document so that that the indentation will be removed before
# processing: place the anchor at the indentation level that you want removed
$var = qq:to/HERE_TARGET/
	your text
	goes here
	HERE_TARGET

# Here document indentation will only be removed by the amount that the anchor
# is removed, preserving poetry :)
$poem = qq:to/EVER_ON_AND_ON/
	Now far ahead the Road has gone,
		And I must follow, if I can,
	Pursuing it with eager feet,
		Until it joins some larger way
	Where many paths and errands meet.
		And whither then? I cannot say.
			--Bilbo in /usr/src/perl/pp_ctl.c
	EVER_ON_AND_ON
say "Here's your poem:\n\n$poem";

