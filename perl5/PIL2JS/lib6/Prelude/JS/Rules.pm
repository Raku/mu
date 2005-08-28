
# This file is an unfinished sketch of :perl5 regex support.

sub JS::Root::rx_(%mods,$re) is primitive {
  my $pattern = $re;
  my $flags = "";
  my $per5 = %mods{'perl5'} || %mods{'Perl5'} || %mods{'P5'};
  die "perl6 rules are not supported" if !$perl5;
  my $global = %mods{'g'} || %mods{'global'};
  $flags .= 'g' if $global;
  $flags .= "i" if %mods{'i'} || %mods{'ignore'};
  $flags .= "m" if %mods{'m'};
  my $matcher = sub ($str) {
    my @ret;
    JS::inline('(
      function (pattern,flags,global,str,ret) {
         var regexp = new RegExp(pattern,flags);
         var match = regex.exec(str);
         if (!match) {}
         else if (global) {
           // XXX - unfinished
         }
         else {
           ret.push(new PIL2JS.Box(match.index));
           for(var i = 0; i < match.length; i++) {
             ret.push(new PIL2JS.Box(match[i]));
           }
         }
      }
    )')($pattern,$flags,$global,$str,@ret);
    if !@ret {
	#my $m = Match.new(0,"",[],undef,undef);
	# $/ = $m;
    } elsif $global {
	# XXX - unfinished
    } else {
	my($from,$m_as_str,@m_as_arrary) = @ret;
	my $to = $from + $m_as_str.chars;
	#my $m = Match.new(1,$m_as_str,@m_as_array,$from,$to);
	# $/ = $m;
    }
  };
  # Rul.new($matcher)  Or just something ~~ will recognize.
}

# $0 etc  should be aliases for $/[0] etc.

