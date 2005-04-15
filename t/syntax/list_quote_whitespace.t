#!/usr/bin/pugs

use v6;
require Test;

=kwid

= DESCRIPTION

Tests that the List quoting parser properly
ignores whitespace in lists. This becomes important
if your line endings are \x0d\x0a.

Characters that should be ignored are:

	\t
	\r
	\n
	\x20
	\xA0

Most likely there are more.

=cut

my @list = <a b c d>;
my @separators = ("\t","\r","\n"," ","\a0");

plan +@separators;

for @separators -> $sep {
  my $str = "<" ~ @list.join($sep) ~ ">";
  my @res = eval $str;
  
  is( @res, @list, "'$sep' is properly parsed as list whitespace")
};