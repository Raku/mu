use v6;

class URI::_query is URI {
  use URI::Escape <uri_unescape>;

  method query() {
    $:str ~~ m,^(<-[?\#]>*)[\?(<-[\#]>*)]?(.*)$,s or die;

    return new Proxy:
      FETCH => { $2 },
      STORE => -> $q is copy {
	$:str = $1;
	if defined $q {
	  $q    ~~ s:g/(<-<URI::uric>>)/%URI::Escape::escapes{$1}/;
	  $:str ~= "?$q";
	}
	$:str ~= $3;
    };
  }

  # Handle ...?foo=bar&bar=foo type of query
  method query_form() {
    my $old = .query;

    return new Proxy:
      FETCH => {
	return if !defined $old || !length $old;
	return unless $old ~~ /=/; # not a form
        split /&/, $old                                  ==>
        map { /=/ ?? pair split /=/, $_, 2 :: $_ => '' } ==>
	map { s:g/\+/ /; uri_unescape $_ }               ==> #/#--vim
	return;
      },
      STORE => -> Pair *@new is copy {
        my @query;
        while my($key,$vals) = splice @new, 0, 2) {
	  $key //= '';
	  $key  ~~ s:g/(<[;\/?:@&=+,\$\[\]%]>)/%URI::Escape::escapes{$1}/;
	  $key  ~~ s:g/ /+/;
	  for $vals -> $val {
	    $val //= '';
	    $val  ~~ s:g/(<[;\/?:@&=+,\$\[\]%]>)/%URI::Escape::escapes{$1}/;
	    $val  ~~ s:g/ /+/;
	    push @query, "$key=$val";
	  }
        }
        .query = @query ?? join '&', @query :: undef;
      };
  }

  # Handle ...?dog+bones type of query
  method query_keywords() {
    return new Proxy:
      FETCH => {
	my $old = .query;
	return if !defined $old;
	return if $old ~~ /=/;  # not keywords, but a form
	map { uri_unescape $_ } split /\+/, $old, -1;
      },
      STORE => -> @copy is rw {
	for @copy -> { s:g/(<[;\/?:@&=+,\$\[\]%]>)/%URI::Escape::escapes{$1}/ }
	.query = @copy ?? join '+', @copy : undef;
      };
  }
}

# Some URI::URL compatibility stuff -- deleted -- Ingo
#*equery = \&query;

1;
