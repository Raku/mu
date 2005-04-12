#!/usr/bin/perl6

use v6;

=head1 Interpolating Functions or Expressions within Strings

You want to expand a function call or expression within a string.

=cut

# You can break your string into concatenated pieces (the long way):
$answer = 'The quick ' ~ func() ~ ' over the lazy dog';

# Or you can interpolate function calls using &function() by using "", qq//,
# or q:f// string delimeters. The function call must begin with ampersand (&)
# and end with paranthesis:
$answer = "STRING &function() MORE STRING";
$answer = qq/STRING &function() MORE STRING/;  # very liberal interpolation
$answer = q:f/STRING &function() MORE STRING"; # interpolates functions only

# To interpolate a class method, using closure curlies:
$answer = "STRING {Dog.bark} MORE STRING";

# And for expressions take the long route:
$phrase = "I have " ~ ($n + 1) ~ " guanacos.";

# Or use closure curlies within "", qq//, or q:c// quoted strings:
$phrase = "I have {$n + 1} guanacos.";
$phrase = qq/I have {$n + 1} guanacos./;      # liberal interpolation
$phrase = q:c/I have {$n + 1} guanacos./;     # interpolation closures only

# Interpolate into a here document:
# XXX: Confirming via p6l that this works as the first param like this:
die "Couldn't send mail" unless send_mail <<qq:to/EOTEXT/, $target
	To: $naughty
	From: Your Bank
	Cc: &get_manager_list($naughty)
	Date: { do { my $now = `date`; chomp $now; $now } } (today)

	Dear $naughty,

	Today, you bounced check number { 500 + int rand(100) } to us.
	Your account is now closed.

	Sincerely,
	the management
	EOTEXT

