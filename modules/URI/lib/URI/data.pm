use v6;

class URI::data isa URI { # RFC 2397
  use MIME::Base64 <encode_base64 decode_base64>;
  use URI::Escape  <uri_unescape>;

  method media_type() {
    my $opaque = .opaque;
    $opaque   ~~ m/^(<-[,]>*),?/ or die;
    my $old    = $1;
    my $base64;
    $base64    = $1 if $old ~~ s:i/(;base64)$//;

    return new Proxy:
      FETCH => {
	return uri_unescape $old if $old;     # media_type can't really be "0"
	return "text/plain;charset=US-ASCII"; # default type
      },
      STORE => -> $new is copy {
	$new    //= "";
	$new     ~~ s:g/%/%25/;
	$new     ~~ s:g/,/%2C/;
	$base64 //= "";
	$opaque  ~~ s/^<-[,]>*,?/$new$base64,/;
	.opaque   = $opaque;
      };
  }

  method data() {
    my ($enc, $data) = split ",", .opaque, 2;

    unless defined $data {
      $data   = "";
      $enc  //= "";
    }

    my $base64 = ($enc ~~ m:i/;base64$/);

    return new Proxy:
      FETCH => { $base64 ?? decode_base64 $data :: uri_unescape $data },
      STORE => -> $new is copy {
	$enc  ~~ s:i/;base64$// if $base64;
	$new //= "";
	my $uric_count = .:uric_count($new);
	my $urienc_len = $uric_count + (length($new) - $uric_count) * 3;
	my $base64_len = int((length($new)+2) / 3) * 4;
	$base64_len    += 7;  # because of ";base64" marker
	if $base64_len < $urienc_len {
	  # XXX - there was a $_[0] here, not
	  # sure what it meant
	  $enc ~= ";base64";
	  $new  = encode_base64 $new, "";
	} else {
	  $new ~~ s:g/%/%25/;
	}
	.opaque = "$enc,$new";
      };
  }

  method :uric_count(Str $str is copy) {
    $str ~~ tr<;/?:@&=+$,\[\]A-Za-z0-9-_.!~*'()>//; #'#--vim
  }
}

1;

__END__

=head1 NAME

URI::data - URI that contains immediate data

=head1 SYNOPSIS

 use URI;

 my URI $u .= new(uri => "data:");
 $u.media_type = "image/gif";
 $u.data = slurp "camel.gif";
 say $u;

=head1 DESCRIPTION

The C<URI::data> class supports C<URI> objects belonging to the I<data>
URI scheme.  The I<data> URI scheme is specified in RFC 2397.  It
allows inclusion of small data items as "immediate" data, as if it had
been included externally.  Examples:

  data:,Perl%20is%20good

  data:image/gif;base64,R0lGODdhIAAgAIAAAAAAAPj8+CwAAAAAI
    AAgAAAClYyPqcu9AJyCjtIKc5w5xP14xgeO2tlY3nWcajmZZdeJcG
    Kxrmimms1KMTa1Wg8UROx4MNUq1HrycMjHT9b6xKxaFLM6VRKzI+p
    KS9XtXpcbdun6uWVxJXA8pNPkdkkxhxc21LZHFOgD2KMoQXa2KMWI
    JtnE2KizVUkYJVZZ1nczBxXlFopZBtoJ2diXGdNUymmJdFMAADs=



C<URI> objects belonging to the data scheme support the common methods
(described in L<URI>) and the following two scheme-specific methods:

=over 4

=item $uri.media_type = $new_media_type

Can be used to get or set the media type specified in the
URI.  If no media type is specified, then the default
C<"text/plain;charset=US-ASCII"> is returned.

=item $uri.data = $new_data

Can be used to get or set the data contained in the URI.
The data is passed unescaped (in binary form).  The decision about
whether to base64 encode the data in the URI is taken automatically,
based on the encoding that produces the shorter URI string.

=back

=head1 SEE ALSO

L<URI>

=head1 COPYRIGHT

Copyright 2005 Ingo Blechschmidt (port to Perl 6)

Copyright 1995-1998 Gisle Aas.

This library is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

=cut
