use v6;

class URI::gopher is URI::_server {
  # <draft-murali-url-gopher>, Dec 4, 1996

  use URI::Escape <uri_unescape>;

  #  A Gopher URL follows the common internet scheme syntax as defined in 
  #  section 4.3 of [RFC-URL-SYNTAX]:
  #
  #        gopher://<host>[:<port>]/<gopher-path>
  #
  #  where
  #
  #        <gopher-path> :=  <gopher-type><selector> | 
  #                          <gopher-type><selector>%09<search> |
  #                          <gopher-type><selector>%09<search>%09<gopher+_string>
  #
  #        <gopher-type> := '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7'
  #                         '8' | '9' | '+' | 'I' | 'g' | 'T'
  #
  #        <selector>    := *pchar     Refer to RFC 1808 [4]
  #        <search>      := *pchar
  #        <gopher+_string> := *uchar  Refer to RFC 1738 [3]
  #        
  #  If the optional port is omitted, the port defaults to 70. 

  method default_port() { 70 }

  method :gopher_type() {
    my $path  = .path_query;
    $path    ~~ s,^/,,;
    my $gtype = $1 if $path ~~ s/^(.)//;

    return new Proxy:
      FETCH => { $gtype },
      STORE => {
	if defined $^new_type {
	  die "Bad gopher type \"^$new_type\""
	    unless length $^new_type == 1;
	  substr($path, 0, 0) = $^new_type;
	  .path_query = $path;
	} else {
	  die "Can't delete gopher type when selector is present"
	    if length $path;
	  .path_query = undef;
	};
  }

  method gopher_type() {
    return new Proxy:
      FETCH => { .:gopher_type // 1 },
      STORE => { .:gopher_type = $^new };
  }

  #*gtype = \&gopher_type;  # URI::URL compatibility
  # Not needed any more

  method selector() is rw { .:gfield(0) }
  method search  () is rw { .:gfield(1) }
  method string  () is rw { .:gfield(2) }

  method :gfield(Int $fno) is rw {
    my $path = .path_query;

    # not according to spec., but many popular browsers accept
    # gopher URLs with a '?' before the search string.
    $path ~~ s/\?/\t/;
    $path  = uri_unescape $path;
    $path ~~ s,^/,,;
    my $gtype = $1 if $path ~~ s,^(.),,;
    my @path = split /\t/, $path, 3;

    return new Proxy:
      FETCH => { @path[$fno] },
      STORE => {
	# modify
	@path[$fno] = $^new;
	pop @path while @path && !defined @path[-1];
	for @path { $_ // = "" }
	$path   = $gtype;
	$path //= "1";
	$path  ~= join "\t", @path;
	.path_query = $path;
      };
  }
}

1;
