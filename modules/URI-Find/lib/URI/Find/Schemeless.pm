module URI::Find::Schemeless-1.8;
class URI::Find::Schemeless is URI::Find;
use v6;

# $Id: Schemeless.pm,v 1.8 2005/03/22 16:03:11 roderick Exp $
#
# Copyright (c) 2000 Michael G. Schwern.  All rights reserved.  This
# program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

rule dns      { <[A-Za-z0-9-]> }
rule cruftSet { <+<URI::cruft>+[<>?]> }

my @tlds = < aero biz com coop edu gov info int mil museum name net org pro >;
rule tld { [ <[a-z]>**{2} | @tlds ] }

=head1 NAME

URI::Find::Schemeless - Find schemeless URIs in arbitrary text.


=head1 SYNOPSIS

  use URI::Find::Schemeless;

  my URI::Find::Schemeless $finder .= new(callback => \&callback);

  The rest is the same as URI::Find.


=head1 DESCRIPTION

URI::Find finds absolute URIs in plain text with some weak heuristics
for finding schemeless URIs.  This subclass is for finding things
which might be URIs in free text.  Things like "www.foo.com" and
"lifes.a.bitch.if.you.aint.got.net".

The heuristics are such that it hopefully finds a minimum of false
positives, but there's no easy way for it know if "COMMAND.COM" refers
to a web site or a file.

=cut

rule schemeless_uri_re {
  # Originally I constrained what couldn't be before the match
  # like this:  don't match email addresses, and don't start
  # anywhere but at the beginning of a host name
  #    (?<![\@.$dnsSet])
  # but I switched to saying what can be there after seeing a
  # false match of "Lite.pm" via "MIME/Lite.pm".
  [ ^ | (?<=<[\s<>()\{\}\[\]]>) ]  # XXX -- fix that (?<=...)
  # hostname
  [ <$dnsSet>+[\.<$dnsSet>+]*\.<tld>
  | [\d**{1,3}\.]{3}**\d{1,3} ] # not inet_aton() complete
  [
      <before <+<space>+<cruft>> # followed by unrelated thing
      (?!\.\w)		         # but don't stop mid foo.xx.bar
                                 # XXX -- fix that (?!...)
      (?<!\.p<[ml]>)             # but exclude Foo.pm and Foo.pl
                                 # XXX -- fix that (<?!...)
      |
      $                          # or end of line
      (?<!\.p<[ml]>)             # but exclude Foo.pm and Foo.pl
                                 # XXX -- fix that (<?!...)
      |
      /<[#]+<uric>>*	         # or slash and URI chars #/#--vim
  ]
};

=head1 AUTHOR

Original code by Roderick Schertler <roderick@argon.org>, adapted by
Michael G Schwern <schwern@pobox.com>.

Currently maintained by Roderick Schertler <roderick@argon.org>.

Ported to Perl 6 by Ingo Blechschmidt <iblech@web.de>.

=head1 SEE ALSO

  L<URI::Find>

=cut

1;
