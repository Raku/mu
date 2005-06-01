module LWP::Simple-0.0.1;
use v6;

# use vars qw($ua %loop_check $FULL_LWP @EXPORT @EXPORT_OK $VERSION);
# .. we won't need these

# @EXPORT = qw(get head getprint getstore mirror);
# @EXPORT_OK = qw($ua);
# How do we get these ?

# I really hate this.  I was a bad idea to do it in the first place.
# Wonder how to get rid of it???  (It even makes LWP::Simple 7% slower
# for trivial tests)
# use HTTP::Status;
# push(@EXPORT, @HTTP::Status::EXPORT);

# $VERSION = sprintf("%d.%02d", q$Revision: 1.41 $ =~ /(\d+)\.(\d+)/);
# $FULL_LWP++ if grep {lc($_) eq "http_proxy"} keys %*ENV;

# my $CRLF = rx:perl5/\015?\012/;
my $CRLF = "\x0D\x0A\x0D\x0A";
my $VERSION = "0.0.1";


sub getprint (Str $url) is export
{
  getstore $url, '';
};

# FIXME to use a callback
sub getstore (Str $url, Str $file) is export
{
  my $fh = open ">$file";
  my $buffer = get $url;
  if (defined $buffer) {
    $fh.print($buffer);
    $fh.close;            # pugs bug
    $buffer;
  };
};

# TODO: Implement a non-faked version
# TODO: Add If-Modified-Since: header
# TODO: Handle 30x Not Modified response
sub mirror ($url, $file)
{
  getstore($url,$file)
}

sub get (Str $url) is export {
  return _trivial_http_get($url);
};

# Refactor common code with _trivial_http_get
sub head (Str $url) is export {
  # * Set a timeout of 60 seconds (however)
  # * Send Connection: close, at least until we know better

  my ($host,$port,$path) = split_uri($url);

  my $req = _make_request( "HEAD", $url );
  my $hdl = _send_request( $host, $port, $req );

  my $head = slurp $hdl;

  # strip away everything except status and headers
  # This should all be done better so the response doesn't live in
  # memory all at once

  if ($head ~~ rx:perl5{^HTTP\/\d+\.\d+\s+(\d+) (?:.*?\015?\012)((?:.*?\015?\012)*?)\015?\012}) {
    my ($code,$head) = ($0,$1);

    # if (want.Boolean) {
    #  return $code ~~ rx:perl5/^2/;
    # }
    # if (want.Int) {
    #  return $code
    # }
    #say $code;
    #say $head;

    # XXX want() is not yet implemented :(
    #if (want.Scalar) {
      return $head
    #};
    # my @list = "X-LWP-HTTP-Status: $code", (split rx:perl5/\015?\012/, $head);
    #if (want.List) { return @list };
    #if (want.Hash) {
    #  my %res = map { rx:perl5/^(.*?): (.*)/; ($0 => $1) } @list;
    #  return %res
    #} else {
    #  # What context can we also get?
    #  return $head;
    #};
  };
};

# Unify with URI.pm
sub split_uri (Str $url) {
  $url ~~ rx:Perl5{^http://([^/:\@]+)(?::(\d+))?(/\S*)?$}; #/#--vim

  my ($host) = $0;
  my ($port) = $1 || 80;
  my ($path) = $2 || "/";

  return ($host,$port,$path);
};

sub _trivial_http_get (Str $url) returns Str {
  # TODO: Set a timeout of 60 seconds (however)
  # TODO: Send Connection: close, at least until we know better
  my ($h,$p,$u) = split_uri($url);

  my $req = _make_request( "GET", $url );
  my $hdl = _send_request( $h, $p, $req );

  # read response+headers:
  # $hdl.irs = /$CRLF$CRLF/; # <-- make this into a todo test

  my $buffer = slurp $hdl;
  # 1 while ( $buffer ~= $hdl.read() and $buffer !~ rx:perl5{$CRLF$CRLF} );
  # my ($status,@headers) = split /$CRLF/, $buffer;
  # worry later about large body

  # strip away status and headers
  # This should all be done better so the response doesn't live in
  # memory all at once

  # if ($buffer ~~ s:perl5{^HTTP\/\d+\.\d+\s+(\d+)([^\012]*?\015?\012)+?\015?\012}{}) {
  if ($buffer ~~ s:Perl5{^HTTP\/\d+\.\d+\s+(\d+)([^\x0A]*?\x0D?\x0A)+?\x0D?\x0A}{}) {
    my $code = $0;

    # XXX: Add 30[1237] checking/recursion

    if ($code ~~ rx:Perl5/^[^2]../) {  # /#--vim
       return ();
    };

    # Later add Content-Size: handling here
  };
  return $buffer
}

sub _make_request (Str $method, Str $uri) {
  my ($h,$p,$u) = split_uri($uri);
  if (%*ENV<HTTP_PROXY>) {
    $u = $uri;
  };

  join "\n", # $CRLF,
    "$method $u HTTP/1.0",
    "Host: $h",
    "User-Agent: lwp-trivial-pugs/$VERSION",
    "Connection: close",
    $CRLF;
};

sub _send_request (Str $host, Int $port, Str $request) {
  # XXX clean up!

  my ($h,$p) = ($host,$port);
  my $http_proxy = %*ENV<HTTP_PROXY> // %*ENV<http_proxy>;
  if defined $http_proxy {
    if $http_proxy ~~ rx:Perl5!http://()(:(\d+))?$! {
      $h = $0;
      $p = $1 || 80;
    } else {
      die "Unhandled/unknown proxy settings: \"$http_proxy\"";
    }
  }

  my $hdl = connect $h, $p;
  $hdl.print($request);
  $hdl.flush;
  $hdl;
};

=pod

=head1 NAME

LWP::Simple - simple procedural interface to LWP

=head1 SYNOPSIS

  pugs -MLWP::Simple -e 'getprint "http://www.sn.no"'

  require LWP::Simple;
  $content = get("http://www.sn.no/");
  die "Couldn't get it!" unless defined $content;

  if (mirror("http://www.sn.no/", "foo") == ???) {
     ...
  }

  if (getprint("http://www.sn.no/")) {
     ...
  }

=head1 DESCRIPTION

This module is meant for people who want a simplified view of the
libwww-perl library.  It should also be suitable for one-liners.  If
you need more control or access to the header fields in the requests
sent and responses received, then you should use the full object-oriented
interface provided by the C<LWP::UserAgent> module.

The following functions are provided (and exported) by this module:

=over 3

=item get($url)

The get() function will fetch the document identified by the given URL
and return it.  It returns C<undef> if it fails.  The $url argument can
be either a simple string or a reference to a URI object.

You will not be able to examine the response code or response headers
(like 'Content-Type') when you are accessing the web using this
function.  If you need that information you should use the full OO
interface (see L<LWP::UserAgent>).

=item head($url)

Get document headers. Depending on the context, C<head> returns
the following values:

=over 4

=item Boolean Context

The HTTP status code (success/failure) is returned

=item Numeric Context

The HTTP status code is returned

=item Scalar Context

All headers are returned as one long string, or maybe as a L<HTTP::Headers>
object, depending on when C<want> gets implemented. Currently, the headers
are returned as one long string.

=item List Context

All headers are returned split into the respective
lines.

=item Hash Context (if it exists?)

A hash is returned mapping the headers. If there are
duplicates, the last one wins.

=back

=item getprint($url)

Get and print a document identified by a URL. The document is printed
to the selected default filehandle for output (normally STDOUT) as
data is received from the network.  If the request fails, then the
status code and message are printed on STDERR.  The return value is
the HTTP response code.

=item getstore($url, $file)

Gets a document identified by a URL and stores it in the file. The
return value is the HTTP response code.

=item mirror($url, $file)

Get and store a document identified by a URL, using
I<If-modified-since>, and checking the I<Content-Length>.  Returns
the HTTP response code.

=back

=head1 INCOMPATIBLE CHANGES

=over 2

=item This Perl6 version of LWP::Simple does not export the HTTP status
codes.

=item (TODO) This module only supports HTTP as the protocol.

=item (TODO) This module does not support proxies.

=back

=head1 SEE ALSO

L<LWP>, L<lwpcook>, L<LWP::UserAgent>, L<HTTP::Status>, L<lwp-request>,
L<lwp-mirror>

=cut
