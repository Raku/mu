use v6;

class URI::ldapi is URI::_ldap is URI::_generic {
  use URI::Escape;

  method un_path() is rw {
    return new Proxy:
      FETCH => { uri_unescape .authority },
      STORE => -> $p is copy {
	$p ~~ s:g/:/%3A/;
	$p ~~ s:g/\@/%40/;
	$self.authority = $p;
      };
  }

  &:nonldap_canonical ::= URI::_generic::canonical; # XXX - correct?
}

1;
