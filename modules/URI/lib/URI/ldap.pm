use v6;

module URI::ldap-1.11;

# Copyright (c) 1998 Graham Barr <gbarr@pobox.com>. All rights reserved.
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.
class URI::ldap isa URI::_ldap isa URI::_server trusts URI {
  method default_port() { 389 }

  method :nonldap_canonical() { .URI::_server::canonical }
  # XXX - correct?
}

1;

__END__

=head1 NAME

URI::ldap - LDAP Uniform Resource Locators

=head1 SYNOPSIS

  use URI;

  my URI $uri .= new(uri => "ldap:$uri_string");
  my $dn     = $uri.dn;
  my $filter = $uri.filter;
  my @attr   = $uri.attributes;
  my $scope  = $uri.scope;
  my %extn   = $uri.extensions;
  
  $uri = URI.new(uri => "ldap:");  # start empty
  $uri.host = "ldap.itd.umich.edu";
  $uri.dn   = "o=University of Michigan,c=US";
  $uri.attributes = <postalAddress>;
  $uri.scope      = "sub";
  $uri.filter     = ("(cn=Babs Jensen)");
  say ~$uri;

=head1 DESCRIPTION

C<URI::ldap> provides an interface to parse an LDAP URI into its
constituent parts and also to build a URI as described in
RFC 2255.

=head1 METHODS

C<URI::ldap> supports all the generic and server methods defined by
L<URI>, plus the following.

Each of the following methods can be used to set or get the value in
the URI. The values are passed in unescaped form.  None of these
return undefined values, but elements without a default can be empty.
If arguments are given, then a new value is set for the given part
of the URI.

=over 4

=item $uri.dn = $new_dn

Sets or gets the I<Distinguished Name> part of the URI.  The DN
identifies the base object of the LDAP search.

=item $uri.attributes = @new_attrs

Sets or gets the list of attribute names which are
returned by the search.

=item $uri.scope = $new_scope

Sets or gets the scope to be used by the search. The value can be one of
C<"base">, C<"one"> or C<"sub">. If none is given in the URI then the
return value defaults to C<"base">.

=item $uri.filter = $new_filter

Sets or gets the filter to be used by the search. If none is given in
the URI then the return value defaults to C<"(objectClass=*)">.

=item $uri.extensions = ($etype => $evalue, ...)

Sets or gets the extensions used for the search. The list passed should
be in the form etype1 => evalue1, etype2 => evalue2,... This is also
the form of list that is returned.

=back

=head1 SEE ALSO

L<RFC-2255|http://www.cis.ohio-state.edu/htbin/rfc/rfc2255.html>

=head1 AUTHOR

Graham Barr E<lt>F<gbarr@pobox.com>E<gt>

Slightly modified by Gisle Aas to fit into the URI distribution.

=head1 COPYRIGHT

Copyright (c) 2005 Ingo Blechschmidt (port to Perl 6).

Copyright (c) 1998 Graham Barr. All rights reserved. This program is
free software; you can redistribute it and/or modify it under the same
terms as Perl itself.

=cut
