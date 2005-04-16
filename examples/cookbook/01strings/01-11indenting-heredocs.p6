#!/usr/bin/perl6

use v6;

=head1 NAME

Perl 6 Cookbook: Indenting Here Documents

=head1 Summary

You want to indent here documents so that it fits well with code, but need the
indentation removed during processing.

=head1 Solution

Perl 6 lets you indent your here documents as much as you want, and then will
remove any indentation up to the amount preceding the terminating string.

	$var = qq:to/HERE_TARGET/
		your text
		goes here
		HERE_TARGET

Since here document indentation will only be removed by the amount that the 
terminating string is indented, poetry is preserved.

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

=cut
