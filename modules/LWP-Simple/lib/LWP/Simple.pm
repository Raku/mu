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
# $FULL_LWP++ if grep {lc($_) eq "http_proxy"} keys %ENV;

# my $CRLF = rx:perl5/\015?\012/;
my $CRLF = "\x0D\x0A\x0D\x0A";
my $VERSION = "0.0.1";


sub getprint ($url)
{ 
  getstore $url, $*OUT;
} 

# FIXME to use a callback
sub getstore ($url, $file)
{ 
  my $fh = open ">$file";
  my $buffer = get $url;
  if (defined $buffer) {
    $fh.print($buffer);
    $fh.close;            # pugs bug
    $buffer;
  };
}

sub mirror ($url, $file)
{ ... }

sub get (Str $url) is export {
  _get($url);
};

sub head (Str $url) is export {
  # * Don't use "say()", be specific and send "\r\n"
  # * Set a timeout of 60 seconds (however)
  # * Make sure the socket is autoflush, or better
  #   is flushed after we've sent our lines.
  # * Send Connection: close, at least until we know better
  # say "$host:$port";
  
  my ($host,$port,$path) = split_uri($url);

  my $req = _make_request( "HEAD", $host, $path );  
  my $hdl = connect($host, $port);
  $hdl.print($req);
  $hdl.flush;
  
  my $head = slurp $hdl;
  
  # strip away everything except status and headers
  # This should all be done better so the response doesn't live in
  # memory all at once
  
  if ($head ~~ rx:perl5{^HTTP\/\d+\.\d+\s+(\d+) (?:.*?\015?\012)((?:.*?\015?\012)*?)\015?\012}) { 
    my ($code,$head) = ($1,$2);

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
    #  my %res = map { rx:perl5/^(.*?): (.*)/; ($1 => $2) } @list;
    #  return %res
    #} else {
    #  # What context can we also get?
    #  return $head;
    #};
  };
};

# Unify with URI.pm
sub split_uri (Str $url) {
  $url ~~ rx:perl5{^http://([^/:\@]+)(?::(\d+))?(/\S*)?$};
  
  my ($host) = $1;
  my ($port) = $2 || 80; 
  my ($path) = $3 || "/";
  
  return ($host,$port,$path);
};

sub _get (Str $url) {
  my ($host,$port,$path) = split_uri($url);
  return _trivial_http_get(($host,$port,$path));
};

sub _trivial_http_get (Str $host, Str $port, Str $path) returns Str {
  # * Don't use "say()", be specific and send "\r\n"
  # * Set a timeout of 60 seconds (however)
  # * Make sure the socket is autoflush, or better
  #   is flushed after we've sent our lines.
  # * Send Connection: close, at least until we know better
  # say "$host:$port";

  my $req = _make_request( "GET", $host, $path );
  
  my $hdl = connect($host, $port);  
  $hdl.print($req);
  $hdl.flush;
  
  # read response+headers:
  # $hdl.irs = /$CRLF$CRLF/; # <-- make this into a todo test

  my $buffer = slurp $hdl;
  # 1 while ( $buffer ~= $hdl.read() and $buffer !~ rx:perl5{$CRLF$CRLF} );
  # my ($status,@headers) = split /$CRLF/, $buffer;
  # worry later about body
  # say $buffer;
  
  # strip away status and headers
  # This should all be done better so the response doesn't live in
  # memory all at once
  
  if ($buffer ~~ s:perl5{^HTTP\/\d+\.\d+\s+(\d+)(.*?\015?\012)+?\015?\012}{}) {  
    my $code = $1;
    
    # XXX: Add 30[1237] checking/recursion

    if ($code ~~ rx:perl5/^[^2]../) {
       return undef;
    };
    
    # strip status and headers
    # $buffer ~~ s:perl5{^.*?$CRLF$CRLF}{};

    # Later add Content-Size: handling here
  };
  return $buffer
}

sub _make_request (Str $method, Str $host, Str $path) {
  join "\n", # $CRLF,
    "$method $path HTTP/1.1",
    "Host: $host",
    "User-Agent: lwp-trivial-pugs/$VERSION",
    "Connection: close",
    $CRLF;
};

1;

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

All headers are returned as one long string

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

This module also exports the HTTP::Status constants and procedures.
You can use them when you check the response code from getprint(),
getstore() or mirror().  The constants are:

   RC_CONTINUE
   RC_SWITCHING_PROTOCOLS
   RC_OK
   RC_CREATED
   RC_ACCEPTED
   RC_NON_AUTHORITATIVE_INFORMATION
   RC_NO_CONTENT
   RC_RESET_CONTENT
   RC_PARTIAL_CONTENT
   RC_MULTIPLE_CHOICES
   RC_MOVED_PERMANENTLY
   RC_MOVED_TEMPORARILY
   RC_SEE_OTHER
   RC_NOT_MODIFIED
   RC_USE_PROXY
   RC_BAD_REQUEST
   RC_UNAUTHORIZED
   RC_PAYMENT_REQUIRED
   RC_FORBIDDEN
   RC_NOT_FOUND
   RC_METHOD_NOT_ALLOWED
   RC_NOT_ACCEPTABLE
   RC_PROXY_AUTHENTICATION_REQUIRED
   RC_REQUEST_TIMEOUT
   RC_CONFLICT
   RC_GONE
   RC_LENGTH_REQUIRED
   RC_PRECONDITION_FAILED
   RC_REQUEST_ENTITY_TOO_LARGE
   RC_REQUEST_URI_TOO_LARGE
   RC_UNSUPPORTED_MEDIA_TYPE
   RC_INTERNAL_SERVER_ERROR
   RC_NOT_IMPLEMENTED
   RC_BAD_GATEWAY
   RC_SERVICE_UNAVAILABLE
   RC_GATEWAY_TIMEOUT
   RC_HTTP_VERSION_NOT_SUPPORTED

The HTTP::Status classification functions are:

=over 3

=item is_success($rc)

True if response code indicated a successful request.

=item is_error($rc)

True if response code indicated that an error occurred.

=back

The module will also export the LWP::UserAgent object as C<$ua> if you
ask for it explicitly.

The user agent created by this module will identify itself as
"LWP::Simple/#.##" (where "#.##" is the libwww-perl version number)
and will initialize its proxy defaults from the environment (by
calling $ua->env_proxy).

=head1 CAVEAT

Note that if you are using both LWP::Simple and the very popular CGI.pm
module, you may be importing a C<head> function from each module,
producing a warning like "Prototype mismatch: sub main::head ($) vs
none". Get around this problem by just not importing LWP::Simple's
C<head> function, like so:

        use LWP::Simple qw(!head);
        use CGI qw(:standard);  # then only CGI.pm defines a head()

Then if you do need LWP::Simple's C<head> function, you can just call
it as C<LWP::Simple::head($url)>.

=head1 INCOMPATIBLE CHANGES

This Perl6 version of LWP::Simple does not export the HTTP status
codes.

=head1 SEE ALSO

L<LWP>, L<lwpcook>, L<LWP::UserAgent>, L<HTTP::Status>, L<lwp-request>,
L<lwp-mirror>
