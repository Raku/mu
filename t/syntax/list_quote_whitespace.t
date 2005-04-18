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

Most likely there are more. James tells me that
the maximum Unicode char is \x10FFFF , so maybe
we should simply (re)construct the whitespace
list via IsSpace or \s on the fly.

Of course, in the parsed result, no item should
contain whitespace.

C<\xA0> is specifically an I<nonbreaking> whitespace
character and thus should B<not> break the list.

=cut

my @list = <a b c d>;
my @separators = ("\t","\r","\n"," ");
my @nonseparators = (",","/","\\",";","\xa0");

plan +@separators + @nonseparators;

for @separators -> $sep {
  my $str = "<" ~ @list.join("$sep$sep") ~ ">";
  my @res = eval $str;

  my $vis = sprintf "%02x", ord $sep;
  is( @res, @list, "'\x$vis\x$vis' is properly parsed as list whitespace")
};

for @nonseparators -> $sep {
  my $ex = @list.join($sep);
  my $str = "<" ~$ex~ ">";
  my @res = eval $str;

  my $vis = sprintf "%02x", ord $sep;
  is( @res, @list.join($sep), "'\x$vis' does not split in a whitespace quoted list")
};
