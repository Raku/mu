use v6;

class URI::news is URI::_server {
  # draft-gilman-news-url-01

  use URI::Escape <uri_unescape>;

  method default_port() { 119 }

  #   newsURL      =  scheme ":" [ news-server ] [ refbygroup | message ]
  #   scheme       =  "news" | "snews" | "nntp"
  #   news-server  =  "//" server "/"
  #   refbygroup   = group [ "/" messageno [ "-" messageno ] ]
  #   message      = local-part "@" domain

  method :group() is rw {
    return new Proxy:
      FETCH => {
	my $old = $.path;
	$old ~~ s,^/,,;
	if not $old ~~ /\@/ and $old ~~ s,/(.*),, {
	  my $extra = $1;
	  return uri_unescape($old), split /-/, $extra;
	}
	return uri_unescape $old;
      },
      STORE => -> $group is copy, $from, $to {
	if $group ~~ /\@/ {
	  $group ~~ s/^\<(.*)\>$/$1/;  # "<" and ">" should not be part of it
	}
	$group ~~ s:g,%,%25,;
	$group ~~ s:g,/,%2F,;
	my $path = $group;
	if defined $from {
	  $path ~= "/$from";
	  $path ~= "-$to" if defined $to;
	}
	.path = $path;
      };
  }

  method group() is rw {
    return new Proxy:
      FETCH => {
	my @old = .:group;
	return if @old[0] =~ m/@/;
	return @old;
      },
      STORE => -> $group, $from, $to {
	die "Group name can't contain \"\@\""
	  if $group =~ m/@/;
	.:group = ($group, $from, $to);
      };
  }

  # XXX - why .:group here? (But I've taken it from the Perl 5 version, so It
  # Is Correct (TM)...)
  method message() {
    return new Proxy:
      FETCH => {
	my $old = .:group;
	return unless $old =~ m/\@/;
	return $old;
      },
      STORE => -> *@args {
	die "Message must contain \"\@\"" unless @args[0] ~~ m/@/;
	.:group = *@args;
      };
  }
}

1;
