use v6;

class URI::mailto isa URI::_query trusts URI { # RFC 2368
  method to() {
    my @old = .headers;

    return new Proxy:
      FETCH => {
	my @old = .headers;
	my @to;
	while @old {
	  my $h = shift @old;
	  my $v = shift @old;
	  push @to, $v  if lc $h eq "to";
	}
	return join ",", @to;
      },
      STORE => {
	my @new = @old;
	# get rid of any other to: fields
	loop my $i = 0; $i < @new; $i += 2 {
	  if lc @new[$i] eq "to" {
	    splice @new, $i, 2;
	    redo;
	  }
	}

	unshift @new, "to" => $^to // "";
	.headers = @new;
      };
  }

  method headers() {
    # The trick is to just treat everything as the query string...
    my $opaque = "to=" . .opaque;
    $opaque   ~~ s/\?/&/;

    return new Proxy:
      # I am lazy today...
      FETCH => { URI.new(uri => "mailto:?$opaque").query_form },
      STORE => -> @new {
	# strip out any "to" fields
	my @to;
	loop my $i = 0; $i < @new; $i += 2 {
	  if lc @new[$i] eq "to" {
	    push @to, (splice @new, $i, 2)[1];  # remove header
	    redo;
	  }
	}

	my $new = join ",", @to;
	$new ~~ s:g/%/%25/;
	$new ~~ s:g/\?/%3F/;
	.opaque     = $new;
	.query_form = @new if @new;
     };
  }
}

1;
