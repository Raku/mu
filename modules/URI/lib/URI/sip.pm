use v6;

module URI::sip-0.10;

# Written by Ryan Kereliuk <ryker@ryker.org>.  This file may be
# distributed under the same terms as Perl itself.
#
# The RFC 3261 sip URI is <scheme>:<authority>;<params>?<query>.
class URI::sip isa URI::_server isa URI::_userpass trusts URI {
  use URI::Escape <uri_unescape>;

  method default_port() { 5060 }

  method authority() is rw {
    $:uri =~ m,^(<URI::scheme>:)?(<-[;?]>*)(.*)$,os or die;

    return new Proxy:
      FETCH => { $2 },
      STORE => -> $auth is rw {
        $:uri = $1 // "";
        my $rest = $3;
        if defined $auth {
	  $auth ~~ s/(<-<URI::uric>>)/%URI::Escape::escapes{$1}/;
	  $:uri ~= "$auth";
        }
        $:uri ~= $rest;
      };
  }

  method params_form() is rw {
    $:uri =~ m,^([<URI::scheme>:)?)[(<-[;?]>*)]?(;<-[?]>*)?(.*)$, or die;
    my $paramstr = $3;

    return new Proxy:
      FETCH => {
	$paramstr ~~ s/^;//;
	return split m/<[;=]>/, $paramstr;
      },
      STORE => -> @args {
        $:uri = $1 ~ $2;
        my $rest = $4;
	my @new;
	loop my $i = 0; $i < @args; $i += 2 {
	  push @new, "@args[$i]=@args[$i+1]";
	}
	$paramstr = join ";", @new;
	$:uri    ~= ";" ~ $paramstr ~ $rest;
      };
  }

  method params() is rw {
    $:uri =~ m,^([<URI::scheme>:]?)[(<-[;?]>*)]?(;<-[?]>*)?(.*)$, or die;
    my $paramstr = $3;

    return new Proxy:
      FETCH => {
	$paramstr ~~ s/^;//;
	return $paramstr;
      },
      STORE => -> $new {
        $:uri = $1 ~ $2;
        my $rest = $4;
	$:uri ~= $paramstr ~ $rest;
      };
  }

  # Inherited methods that make no sense for a SIP URI.
  method path           {}
  method path_query     {}
  method path_segments  {}
  method abs($self:)    { $self }
  method rel($self:)    { $self }
  method query_keywords {}
}

1;
