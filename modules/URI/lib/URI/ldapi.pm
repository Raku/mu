use v6;

class URI::ldapi isa URI::_ldap isa URI::_generic trusts URI {
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
