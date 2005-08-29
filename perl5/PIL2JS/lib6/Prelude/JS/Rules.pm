
# This file is an unfinished sketch of :perl5 regex support.

sub JS::Root::rx_(%mods, Str $re, Str $qo, Str $qc) is primitive {
  my $pattern = $re;
  my $flags   = "";
  my $perl5   = %mods{'perl5'} || %mods{'Perl5'} || %mods{'P5'};
  die "Perl 6 Rules are not supported." if !$perl5;

  my $global = %mods{'g'} || %mods{'global'};
  $flags ~= 'g' if $global;
  $flags ~= "i" if %mods{'i'} || %mods{'ignore'};
  $flags ~= "m" if %mods{'m'};

  my $matcher = sub ($str) {
    my @ret = JS::inline('(
      function (pattern,flags,global,str) {
        var ret    = [];

        var regexp = new RegExp(pattern,flags);
        var match  = regexp.exec(str);

        if (!match) {
        }
        else if (global) {
          // XXX - unfinished
        }
        else {
          ret.push(match.index);
          for(var i = 0; i < match.length; i++) {
            ret.push(match[i]);
          }
        }

        return ret;
      }
    )')(~$pattern,$flags,$global,$str);

    my $new_match = JS::inline('PIL2JS.toPIL2JSBox(function (ok, from, to, str, subpos, subnamed) {
      return new PIL2JS.Match(ok, from, to, str, subpos, subnamed);
    })');

    if !@ret {
	my $m  = $new_match(?0);
	$/    := $m;
    } elsif $global {
	# XXX - unfinished
    } else {
	my $from       = shift @ret;
        my $m_as_str   = shift @ret;
        my @m_as_array = @ret;
	my $to = $from + $m_as_str.chars;
	my $m  = $new_match(?1, $from, $to, $m_as_str, @m_as_array, []);
        $/    := $m;
    }
  };

  JS::inline('(function (matcher) { return new PIL2JS.Rul(matcher) })')($matcher);
}

method JS::Root::matcher (Rul $rule:) {
  JS::inline('(function (rul) { return rul.matcher })')($rule);
}
for <ok from to str subpos subnamed> -> $attr {
  Pugs::Internals::eval "method JS::Root::$attr (Match \$match:) \{
    JS::inline('(function (match) \{ return match.$attr \})')(\$match);
  \}";
}

multi sub infix:«~~» (Str $str, Rul $rule) is primitive {
  $rule.matcher.($str);
}

# Needs PIL2 and MMD to be done without hacks
sub PIL2JS::Internals::Hacks::postcircumfix_for_match_objects (
  Match $match, Int *@idxs
) is primitive {
  $match.subpos[*@idxs];
}

sub PIL2JS::Internals::Hacks::init_match_postcircumfix_method () is primitive {
  JS::inline('(function () {
    PIL2JS.addmethod(
      _3amain_3a_3aMatch,
      "postcircumfix:[]",
      _26PIL2JS_3a_3aInternals_3a_3aHacks_3a_3apostcircumfix_for_match_objects
    );
  })')();
}

# $0 etc. are aliases for $/[0] etc.
