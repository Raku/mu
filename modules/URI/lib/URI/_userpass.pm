use v6;

class URI::_userpass {
  use URI::Escape <uri_unescape>;

  method user() is rw {
    my $info = .userinfo;

    return new Proxy:
      FETCH => {
	return unless defined $info;
	$info ~~ s/:.*//;
	return uri_unescape $info;
      },
      STORE => -> $new is copy {
	my $pass = $info // "";
	$pass ~~ s/^<-[:]>*//;

	if not defined $new and not length $pass {
	  .userinfo = undef;
	} else {
	  $new //= "";
	  $new ~~ s:g/%/%25/;
	  $new ~~ s:g/:/%3A/;
	  .userinfo = "$new$pass";
	}

	return unless defined $info;
	$info ~~ s/:.*//;
	return uri_unescape $info;
      };
  }

  method password() is rw {
    my $info = .userinfo;

    return new Proxy:
      FETCH => {
	return unless defined $info;
	return unless $info ~~ s/^<-[:]>*://;
	return uri_unescape $info;
      },
      STORE => -> $new is copy {
	my $user = $info // "";
	$user ~~ s/:.*//;

	if not defined $new and not length $user {
	  .userinfo = undef;
	} else {
	  $new //= "";
	  $new ~~ s:g/%/%25/;
	  .userinfo = "$user:$new";
	}
      };
  }
}

1;
