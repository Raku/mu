use v6;

moulde URI::_ldap-1.10;

# Copyright (c) 1998 Graham Barr <gbarr@pobox.com>. All rights reserved.
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.
class URI::_ldap is URI {
  use URI::Escape <uri_unescape>;

  method :ldap_elem($elem) is rw {
    my $query = .query;
    my @bits  = (split /\?/, $query // ""), ("") xx 4);
    my $old   = @bits[$elem];

    return new Proxy:
      FETCH => { $old },
      STORE => -> $new is copy {
	$new ~~ s:g/\?/%3F/;
	@bits[$elem] = $new;
	$query = join "?", @bits;
	$query ~~ s/\?+$//;
	$query = undef unless length $query;
	.query = $query;
      };
  }

  method dn() is rw {
    return new Proxy:
      FETCH => { my $old = .path; $old ~~ s:^/::; return uri_unescape $old },
      STORE => { .path = $^new };
  }

  method attributes() is rw {
    return new Proxy:
      FETCH => { my $old = .:ldap_elem(0); map { uri_unescape($_) } split /,/, $old },
      STORE => {
	.:ldap_elem(0) =
	  join ",",
	  map { my $tmp = $_; $tmp ~~ s:g/,/%2C/; $tmp } *@^args;
      };
  }

  method :scope() {
    return new Proxy:
      FETCH => {
	my $old = .:ldap_elem(1);
	return defined $old ?? uri_unescape $old :: undef;
      },
      STORE => { .:ldap_elem(1) = $^new };
  }

  method scope() {
    return new Proxy:
      FETCH => { length .:scope ?? .:scope :: "base" },
      STORE => { .:scope = $^new };
  }

  method :filter() {
    return new Proxy:
      FETCH => {
	return unless defined my $old = .:ldap_elem(2);
	return uri_unescape $old;
      },
      STORE => { .:ldap_elem(2) = $^new };
  }

  method filter() {
    return new Proxy:
      FETCH => { length .:filter ?? .:filter :: "(objectClass=*)" },
      STORE => { .:filter = $^new };
  }

  method extensions() {
    return new Proxy:
      FETCH => {
	map { uri_unescape $_     } <==
	map { m/^(<-[=]>+)=(.*)$/ } <==
	split m/,/, .:ldap_elem(3);
      },
      STORE => -> Pair *@args is copy {
	while @args {
	  my ($key, $value) = shift.kv;
	  push @ext,
	    join "=", map { $_ //= ""; s:g/,/%2C/g; $_ } $key, $value;
	}
	.:ldap_elem(3) = @ext;
      };
  }

  method canonical() {
    my $other = .:nonldap_canonical;

    # The stuff below is not as efficient as one might hope...

    $other .= clone if $other =:= $self;

    $other.dn = .:normalize_dn($other.dn);

    # Should really know about mixed case "postalAddress", etc...
    $other.attributes = map { lc $_ } $other.attributes;

    # Lowecase scope, remove default
    my $old_scope = $other.scope;
    my $new_scope = lc $old_scope;
    $new_scope = "" if $new_scope eq "base";
    $other.scope = $new_scope if $new_scope ne $old_scope;

    # Remove filter if default
    my $old_filter = $other.filter;
    $other.filter = "" if
      lc $old_filter eq "(objectclass=*)" or
      lc $old_filter eq "objectclass=*";

    # Lowercase extensions types and deal with known extension values
    my @ext = $other.extensions;
    loop my $i = 0; $i < @ext; $i += 2 {
      my $etype = @ext[$i] = lc @ext[$i];
      if $etype =~ m/^!?bindname$/ {
	@ext[$i+1] = .:normalize_dn(@ext[$i+1]);
      }
    }
    $other.extensions = @ext if @ext;
    
    return $other;
  }

  method .:normalize_dn($dn) { # RFC 2253
    my $dn = shift;

    return $dn;
    # The code below will fail if the "+" or "," is embedding in a quoted
    # string or simply escaped...

    my @dn = split /(<[+,]>)/, $dn;
    for @dn {
      s/^(<[a-zA-Z]>+=)/{lc $1}/;
    }
    return join "", @dn;
  }
}

1;
