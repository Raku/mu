use v6;

class URI::ftp isa URI::_server isa URI::_userpass trusts URI {
  method default_port() { 21 }

  &path ::= &path_query;   # XXX

  &:user     ::= &SUPER::user;     # XXX - correct?
  &:password ::= &SUPER::password; # XXX - correct?

  method user() is rw {
    return new Proxy:
      FETCH => { .user           // "anonymous" },
      STORE => { (.user = $^new) // "anonymous" };
  }

  method password() is rw {
    my $fetch = {
      my $pass = .:password;
      unless defined $pass {
	my $user = .user;
	if $user eq "anonymous"|"ftp" {
	  # anonymous ftp login password
	  # If there is no ftp anonymous password specified
	  # then we'll just use 'anonymous@'
	  # We don't try to send the read e-mail address because:
	  # - We want to remain anonymous
	  # - We want to stop SPAM
	  # - We don't want to let ftp sites to discriminate by the user,
	  #   host, country or ftp client being used.
	  $pass = 'anonymous@';
	}
      }
      return $pass;
    };

    return new Proxy:
      FETCH => $fetch,
      STORE => { .:password = $^new; $fetch.() };
  }
}

1;
