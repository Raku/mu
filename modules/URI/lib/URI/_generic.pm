use v6;

class URI::_generic isa URI isa URI::_query trusts URI {
  use URI::Escape <uri_unescape>;

  # XXX?
  my $ACHAR = $URI::uric;  $ACHAR =~ s,\\[/?],,g;
  my $PCHAR = $URI::uric;  $PCHAR =~ s,\\[?],,g;

  method :no_scheme_ok() { 1 }

  method authority() is rw {
    $:str ~~ m,^([<URI::scheme>:]?)[//(<-[/?\#]>*)]?(.*)$, or die;

    return new Proxy:
      FETCH => {
	return $2; # XXX - this $2 should reference the $2 of the match above.
      },
      STORE => -> Str $auth is copy {
	$:str = $1;
	my $rest = $3;
	$auth ~~ s:g/(<-<achar>>)/%URI::Escape::escapes{$1}/;
	$:str ~= "//$auth";
	$rest = .:check_path($rest, $:str);
	$:str ~= $rest;
	return $2;
      };
  }

  method path() is rw {
    $:str =~ m,^([<-[:/?\#]>+:]?[//<-[/?\#]>*]?)(<-[?\#]>*)(.*)$, or die;

    return new Proxy:
      FETCH => { $2 },
      STORE => -> $new_path is rw {
	$:str = $1;
	my $rest = $3;
	$new_path //= "";
	$new_path ~~ s:g/(<-<pchar>>)/%URI::Escape::escapes{$1}/;
	$new_path = .:check_path($new_path, $:str);
	$:str ~= $new_path ~ $rest;
	return $2;
      };
  }

  method path_query() is rw {
    $:str ~~ m,^([<-[:/?\#]>+:]?[//<-[/?\#]>*]?)(<-[\#]>*)(.*)$,s or die;

    return new Proxy:
      FETCH => { $2 },
      STORE => -> $new_path is rw {
	$:str = $1;
	my $rest = $3;
	$new_path //= "";
	$new_path ~~ s:g/(<-<URI::uric>>)/%URI::Escape::escapes{$1}/;
	$new_path = .:check_path($new_path, $:str);
	$:str ~= $new_path ~ $rest;
	return $2;
      };
  }

  method :check_path(Str $path is copy, Str $pre) {
    my $prefix;
    if $pre ~~ m,/, {  # authority present
      $prefix = "/" if length $path and not $path ~~ m,^<[/?\#]>,;
    } else {
      if $path ~~ m,^//, {
	warn "Path starting with double slash is confusing";
      } elsif not length $pre and $path ~~ m,^<-[:/?\#]>+:, {
	warn "Path might look like scheme, './' prepended";
	$prefix = "./";
      }
    }

    substr($path, 0, 0) = $prefix if defined $prefix;
    return $path;
  }

  method path_segments() is rw {
    return new Proxy:
      FETCH => {
	split("/", $path, -1) ==>
	map:{m/;/ ?? .:split_segment($^a) :: uri_unescape($^a) } ==>
	return;
      },
      STORE => -> @arg {
	my $path = .path;

	for @arg -> $_ is rw {
	  if $_ ~~ List {
	    my @seg = @$_;
	    @seg[0] ~~ s:g/%/%25/;
	    for @seg { s:g/;/%3B/ }
	    $_ = join ";", @seg;
	  } else {
	    $_ .= trans [<% ;>] => [<%25 %3B>];
	  }
	  s:g,/,%2F,g;
	}
	.path = join "/", @arg;

	split("/", $path, -1) ==>
	map:{m/;/ ?? .:split_segment($^a) :: uri_unescape($^a) } ==>
	return;
      };
  }


  method :split_segment(Str $str) {
    use URI::_segment;
    return new URI::_segment: segment => $str;
  }


  method abs(::?CLASS $self: Str|URI $base is copy) {
    $base = URI.new(uri => $base) unless $base ~~ URI;

    if my $scheme = .scheme {
      return $self unless $URI::ABS_ALLOW_RELATIVE_SCHEME;
      return $self unless $scheme eq $base.scheme;
    }

    my $abs = .clone;
    $abs.scheme($base.scheme);
    return $abs if $:str ~~ m,^[<URI::scheme>:]?//,;
    $abs.authority = $base.authority;

    return $abs if $.path ~~ m,^/,;

    unless length $.path {
      my $abs = $base.clone;
      my $query = .query;
      $abs.query = $query if defined $query;
      $abs.fragment = .fragment;
      return $abs;
    }

    my $p = $base.path;
    $p ~~ s,<-[/]>+$,,;
    $p ~= $path;
    my @p = split "/", $p, -1;
    shift @p if @p and not length @p[0];
    my $i = 1;
    while $i < @p {
      #print "$i ", join("/", @p), " (@p[$i])\n";
      if @p[$i-1] eq "." {
	splice @p, $i-1, 1;
	$i-- if $i > 1;
      } elsif (@p[$i] eq ".." and @p[$i-1] ne ".." {
	splice @p, $i-1, 2;
	if $i > 1 {
	  $i--;
	  push @p, "" if $i == @p;
	}
      } else {
	$i++;
      }
    }
    @p[-1] = "" if @p and @p[-1] eq ".";  # trailing "/."
    if $URI::ABS_REMOTE_LEADING_DOTS {
      shift @p while @p and @p[0] ~~ /^\.\.?$/;
    }
    $abs.path = "/" . join "/", @p;
    return $abs;
  }

  # The oposite of $url->abs.  Return a URI which is as relative as possible
  method rel(Str|URI $base) {
    my $rel = .clone;
    $base = URI.new(uri => $base) unless $base ~~ URI;

    #my($scheme, $auth, $path) = @{$rel}{qw(scheme authority path)};
    my ($scheme, $auth, $path) = ($rel.scheme, $rel.canonical.authority, $rel.path);

    if not defined $scheme && not defined $auth {
      # it is already relative
      return $rel;
    }

    my ($bscheme, $bauth, $bpath) = ($base.scheme, $base.canonical.authority, $base.path);

    $_ //= "" for $bscheme, $bauth, $auth;

    unless $scheme eq $bscheme and $auth eq $bauth {
      # different location, can't make it relative
      return $rel;
    }

    for $path, $bpath { $_ = "/$_" unless m,^/, }

    # Make it relative by eliminating scheme and authority
    $rel.scheme    = undef;
    $rel.authority = undef;

    # This loop is based on code from Nicolai Langfeldt <janl@ifi.uio.no>.
    # First we calculate common initial path components length ($li).
    my $li = 1;
    while 1 {
      my $i = index $path, '/', $li;
      last if $i < 0 or
	      $i != index $bpath, '/', $li or
	      substr($path,$li,$i-$li) ne substr($bpath,$li,$i-$li);
      $li = $i+1;
    }
    # then we nuke it from both paths
    substr($path, 0,$li) = '';
    substr($bpath,0,$li) = '';

    if
      $path eq $bpath and
      defined $rel.fragment and
      not defined $rel.query
    {
      $rel.path = "";
    } else {
      # Add one "../" for each path component left in the base path
      $path = ('../' xx ($bpath ~~ tr|/|/|)) ~ $path;
      $path = "./" if $path eq "";
      $rel.path = $path;
    }

    return $rel;
  }
}

1;
