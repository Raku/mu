#!/usr/bin/perl6

use v6;

=head1 NAME

Perl 6 Cookbook: Escaping Characters

=head1 Summary

Sometimes strings need to have special characters escaped before operations are
performed, or before output to another script or program.

=head1 Solution

Use a subsitution rule to escape characters. For HTML encoding, use the
HTML::Entities module.

	# backslash escaping
	$var ~~ s:g/(<[CHARLIST]>)/\\$1/;

	# double-character escaping
	$var ~~ s:g/(<[CHARLIST]>)/$1$1/;

XXX Oh, (<[]>) is just horrible!

=head1 Details

For a quick escaping of one character:

	$var ~~ s:g/%/%%/g;

Escaping quotes for the shell:

	$string ~ q/Mom said, "Don't do that."/;
	$string ~~ s:g/(['"])/\\$1/;

XXX Don't use this example, or explicitly note that the result is UNSAFE for
shell usage. People might get the impression that quote escaping is enough.

Escaping for VMS DCL:

	$string = q(Mom said, "Don't do that.");
	$string ~~ s:g/(<['"]>)/$1$1/g;

Escape anything besides A-Z:

	# TODO: Keep up to date with moving target of range and 
	# negative matching
	$string ~~ s:g/(<-[A..Z]>)/\\$1/;

You can escape all non-word metacharacters using the quotemeta function:

	# both equivalent:
	$string ~ "this is\\ a\\ test\\!";
	$string ~ "this " ~ quotemeta("is a test!");

To interpolate quotemeta directly into a string, use a closure:

	# with liberal interpolation
	$string = "this {quotemeta('is a test!')}";
	# with closure-only interpolation
	$string = q:c/this {quotemeta('is a test!'}/;

To encode a string for HTML, use the encode_entities() function from
HTML::Entities: (See XXX for more discussion.)

	use HTML::Entities;
	my $string = "Våre norske tegn bør æres"; 
	say encode_entities($string);
	# output: "V&aring;re norske tegn b&oslash;r &#230;res";

TODO: Add an example for safe SQL quoting XXX Or not. This "safe" thing is
really dangerous, as it strengthens the beliefs of naive programmers, and
discussion of what really needs to be done is too in depth for this document.

=cut
