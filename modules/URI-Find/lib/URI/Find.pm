module URI::Find-0.15;
class URI::Find;
use v6;

# $Id: Find.pm,v 1.15 2005/03/22 16:03:09 roderick Exp $
#
# Copyright (c) 2000 Michael G. Schwern.  All rights reserved.  This
# program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

use URI;

rule uri            { [ <real_uri> | <schemeless_uri> ] }
rule real_uri       { <URI::scheme> <[:]> <uric_cheat> <+<URI::uric>+[#]>* }
rule schemeless_uri { \b\B }
rule uric_cheat     { <+<uric>-[:]> }
rule cruft          { <[\]),.'";]> } #'#--vim

=head1 NAME

  URI::Find - Find URIs in arbitrary text


=head1 SYNOPSIS

  use URI::Find;

  my URI::Find $finder .= new(callback => -> $uri {...});

  my $how_many_found = $finder.find($text);


=head1 DESCRIPTION

This module does one thing: Finds URIs and URLs in plain text.  It finds
them quickly and it finds them B<all> (or what URI::URL considers a URI
to be.)  It only finds URIs which include a scheme (http:// or the
like), for something a bit less strict have a look at
L<URI::Find::Schemeless|URI::Find::Schemeless>.

For a command-line interface, see Darren Chamberlain's C<urifind>
script.  It's available from his CPAN directory,
L<http://www.cpan.org/authors/id/D/DA/DARREN/>.

=head2 Public Methods

=over 4

=item B<new>

  my URI::Find $finder .= new(callback => \&callback);

Creates a new URI::Find object.

&callback is a function which is called on each URI found.  It is
passed two arguments, the first is a URI::URL object representing the
URI found.  The second is the original text of the URI found.  The
return value of the callback will replace the original URI in the
text.

=cut

has Code $:callback;
has Str ($:start_cruft, $:end_cruft);
submethod BUILD($:callback) {}

=item B<find>

  my $how_many_found = $finder->find(\$text);

$text is a string to search and possibly modify with your callback.

=cut

method find(Str $test) {
  my $urlsfound = 0;

  # Yes, evil.  Basically, look for something vaguely resembling a URL,
  # then hand it off to URI::URL for examination.  If it passes, throw
  # it to a callback and put the result in its place.
  my $uri_cand;
  my $uri;

  $text ~~ s:g[(\< <uri> \> | <uri>)][{
    my $orig_match = $1;

    # A heruristic.  Often you'll see things like:
    # "I saw this site, http://www.foo.com, and its really neat!"
    # or "Foo Industries (at http://www.foo.com)"
    # We want to avoid picking up the trailing paren, period or comma.
    # Of course, this might wreck a perfectly valid URI, more often than
    # not it corrects a parse mistake.
    $orig_match = .:decruft($orig_match);

    if my $uri = .:is_uri(\$orig_match) { # Its a URI.
      $urlsfound++;

      # Don't forget to put any cruft we accidentally matched back.
      .:recruft($:callback($uri, $orig_match));
    }
    else {                        # False alarm.
      # Again, don't forget the cruft.
      .:recruft($orig_match);
    }
  }];

  return $urlsfound;
}

=back

=head2 Protected Methods

I got a bunch of mail from people asking if I'd add certain features
to URI::Find.  Most wanted the search to be less restrictive, do more
heuristics, etc...  Since many of the requests were contradictory, I'm
letting people create their own custom subclasses to do what they
want.

The following are methods internal to URI::Find which a subclass can
override to change the way URI::Find acts.  They are only to be called
B<inside> a URI::Find subclass.  Users of this module are NOT to use
these methods.

=over

=cut

=item B<:decruft>

  my $uri = .:decruft($uri);

Sometimes garbage characters like periods and parenthesis get
accidentally matched along with the URI.  In order for the URI to be
properly identified, it must sometimes be "decrufted", the garbage
characters stripped.

This method takes a candidate URI and strips off any cruft it finds.

=cut

method decruft(Str $orig_match) {
  $:start_cruft = '';
  $:end_cruft   = '';

  if $orig_match ~~ s/(<cruft>+)$// {
    $:end_cruft = $1;
  }

  return $orig_match;
}

=item B<recruft>

  my $uri = .:recruft($uri);

This method puts back the cruft taken off with decruft().  This is necessary
because the cruft is destructively removed from the string before invoking
the user's callback, so it has to be put back afterwards.

=cut

#'#

method recruft(Str $uri) {
  return $:start_cruft ~ $uri ~ $:end_cruft;
}

=item B<schemeless_to_schemed>

  my $schemed_uri = $self->schemeless_to_schemed($schemeless_uri);

This takes a schemeless URI and returns an absolute, schemed URI.  The
standard implementation supplies ftp:// for URIs which start with ftp.,
and http:// otherwise.

=cut

method :schemeless_to_schemed(Str $uri_cand) {
  $uri_cand ~~ s|^(\<?)ftp\.|$1ftp://ftp\.| or
  $uri_cand ~~ s|^(\<?)|${1}http://|;

  return $uri_cand;
}

=item B<is_schemed>

  $obj->is_schemed($uri);

Returns whether or not the given URI is schemed or schemeless.  True for
schemed, false for schemeless.

=cut

method :is_schemed(Str $uri) {
    return $uri ~~ /^\<?<scheme>:/;
}

=back

=head1 EXAMPLES

Simply print the original URI text found and the normalized
representation.

  my URI::Find $finder .= new(callback => sub ($uri, $orig_uri) {
    say "The text '$orig_uri' represents '$uri'.";
    return $orig_uri;
  });
  $finder.find($text);

Check each URI in document to see if it exists.

  use LWP::Simple;

  my URI::Find $finder .= new(callback => -> $uri, $orig_uri {
    if head $uri {
      say "'$orig_uri' is okay.";
    } else {
      say "'$orig_uri' cannot be found.";
    }
    $orig_uri;
  });
  $finder.find($text);


Turn plain text into HTML, with each URI found wrapped in an HTML anchor.

  use CGI::Lite <browser_escape>;

  $text = "<pre>\n{browser_escape $text}</pre>\n";
  my URI::Find $finder -= new(callback => sub ($uri, $orig_uri) {
    return "<a href=\"$uri\">$orig_uri</a>";
  });
  $finder.find($text);


=head1 CAVEATS, BUGS, ETC...

RFC 2396 Appendix E suggests using the form '<http://www.foo.com>' or
'<URL:http://www.foo.com>' when putting URLs in plain text.  URI::Find
accomidates this suggestion and considers the entire thing (brackets
and all) to be part of the URL found.  This means that when
find_uris() sees '<URL:http://www.foo.com>' it will hand that entire
string to your callback, not just the URL.

NOTE:  The prototype on find_uris() is already getting annoying to me.
I might remove it in a future version.


=head1 SEE ALSO

  L<URI::Find::Schemeless>, L<URI::URL>, L<URI>,
  RFC 2396 (especially Appendix E)


=head1 AUTHOR

Michael G Schwern <schwern@pobox.com> with insight from Uri Gutman,
Greg Bacon, Jeff Pinyan, Roderick Schertler and others.

Currently maintained by Roderick Schertler <roderick@argon.org>.

Port to Perl 6 by Ingo Blechschmidt <iblech@web.de>.

=cut


method :is_uri(Str $uri is copy) {
  # Translate schemeless to schemed if necessary.
  $uri = .:schemeless_to_schemed($uri) unless .:is_schemed($uri);

  $uri = URI.new($uri);

  if $uri->scheme { # Its a URI.
    return $uri;
  } else {           # leave everything untouched, its not a URI.
    return false;
  }
}

1;
