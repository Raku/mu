use v6;

class URI::pop isa URI::_server trusts URI { # RFC 2384
  use URI::Escape <uri_unescape>;

  method default_port() { 110 }

  #pop://<user>;auth=<auth>@<host>:<port>

  method user() is rw {
    return new Proxy:
      FETCH => {
	my $old = $self->userinfo;
	return unless defined $old;
	$old ~~ s/;.*//;
	return uri_unescape $old;
      },
      STORE => -> $new is copy {
	my $new_info = $old;
	$new_info  //= "";
	$new_info   ~~ s/^<-[;]>*//;

	if !defined $new && !length $new_info {
	  $self.userinfo = undef;
	} else {
	  $new //= "";
	  $new ~~ s:g/%/%25/;
	  $new ~~ s:g/;/%3B/;
	  $self.userinfo = "$new$new_info";
	}
      };
  }

  method auth() is rw {
    return new Proxy:
      FETCH => {
	my $old = $self->userinfo;
	return unless defined $old;
	$old ~~ s/^<-[;]>*//;
	return uri_unescape $1 if $old ~~ m:i/;auth=(.*)/;
	return;
      },
      STORE => -> $auth is copy {
	my $new  = $old;
	$new   //= "";
	$new    ~~ s/(^<-[;]>*)//;
	my $user = $1;
	$new    ~~ s:i/;auth=<-[;]>*//;

	if defined $auth {
	  $auth ~~ s:g/%/%25/;
	  $auth ~~ s:g/;/%3B/;
	  $new   = ";AUTH=$auth$new";
	}

	$self.userinfo = "$user$new";
      };
  }
}

1;
