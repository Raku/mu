use v6;

class URI::_server isa URI::_generic trusts URI {
  method userinfo() is rw {
    my $old = .authority;

    return new Proxy:
      FETCH => {
	return undef if not defined $old or not $old ~~ m/(.*)@/;
	return $1;
      },
      STORE => -> $ui is copy {
	my $new = $old;
	$new //= "";
	$new ~~ s/.*@//;  # remove old stuff
	if defined $ui {
	  $ui ~~ s:g/@/%40/g;
	  $new = "$ui\@$new";
	}
	.authority = $new;

	return undef if not defined $old or not $old ~~ m/(.*)@/;
	return $1;
      };
    };
  }

  method host() is rw {
    my $old = .authority;

    return new Proxy:
      FETCH => {
	return undef unless defined $old;
	$old ~~ s/.*@//;
	$old ~~ s/:\d+$//;
	return uri_unescape $old;
      },
      STORE => -> $new is copy {
	my $tmp = $old;
	$tmp //= "";
	my $ui   = ($tmp ~~ m/(.*@)/)   ?? $1 :: "";
	my $port = ($tmp ~~ m/(:\d+)$/) ?? $1 :: "";
	if length $new {
	  $new ~~ s:g/@/%40/g;   # protect @
	  $port = $1 if $new ~~ m:s/(:\d+)$//;
	}
	.authority = "$ui$new$port";
      };
  }

  method :port() is rw {
    my $old = .authority;

    return new Proxy:
      FETCH => {
	return $1 if defined $old and $old ~~ m/:(\d*)$/;
	return;
      },
      STORE => {
	my $new = $old;
	$new ~~ s/:\d*$//;
	$new .= ":$^port" if defined $^port;
	.authority = $new;
	return $1 if defined $old and $old ~~ m/:(\d*)$/;
	return;
      };
  }

  method port() is rw {
    return new Proxy:
      FETCH => {                  .:port || .default_port },
      STORE => { .:port = $^port; .:port || .default_port };
  }

  method host_port() is rw {
    my $old = .authority;

    my $fetch = {
      return undef unless defined $old;
      $old ~~ s/.*@//;        # zap userinfo
      $old ~~ s/:$//;         # empty port does not could
      $old ~= ":" ~ .port unless $old ~~ /:/;
      return $old;
    };

    return new Proxy:
      FETCH => $fetch,
      STORE => { .host = $^new; $fetch.() };
  }

  method default_port() { undef }

  method canonical() {
    my $other    = .SUPER::canonical; # XXX - correct?
    my $host     = $other.host || "";
    my $port     = $other.:port;
    my $uc_host  = $host ~~ m/[A-Z]/;
    my $def_port =
      defined $port && ($port eq "" || $port == .default_port);

    if $uc_host or $def_port {
      $other .= clone if $other =:= $self;
      $other.host = lc $host if $uc_host;
      $other.port = undef    if $def_port;
    }

    return $other;
  }
}

1;
