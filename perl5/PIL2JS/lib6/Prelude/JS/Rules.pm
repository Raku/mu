
# This file is an unfinished sketch of :perl5 regex support.

sub JS::Root::rxbare_(Str $re) is primitive {
    my %h;
    rx_helper_(%h, $re, "/", "/");
}
sub JS::Root::rx_(%mods, Str $re, Str $qo, Str $qc) is primitive {
    rx_helper_(%mods, $re, $qo, $qc);
}
sub JS::Root::m_(%mods, Str $re, Str $qo, Str $qc) is primitive {
    rx_helper_(%mods, $re, $qo, $qc);
}

sub JS::Root::rx_helper_(%mods, Str $re, Str $qo, Str $qc) is primitive {
  my $pattern = $re;
  my $flags   = "";
  my $perl5   = %mods{'perl5'} || %mods{'Perl5'} || %mods{'P5'};
  die "Perl 6 Rules are not supported." if !$perl5;

  my $global = %mods{'g'} || %mods{'global'};
  $flags ~= "i" if %mods{'i'} || %mods{'ignore'};
  $flags ~= "m" if %mods{'m'};

  my $new_Match = JS::inline('
     PIL2JS.toPIL2JSBox(function (as_b, from, to, as_s, as_a, as_h) {
        return new PIL2JS.Match(as_b, from, to, as_s, as_a, as_h);
     })');

  my $matcher;
  if $global {
    my %mods_no_g = %mods;
    %mods_no_g.delete('g');
    %mods_no_g.delete('global');
    my $rx = rx_helper_(%mods_no_g,$re,$qo,$qc);
    $matcher = sub ($string) {
      my $s = ~$string;
      my $offset = 0;
      my @a;
      while 1 {
        my $m = $s ~~ $rx;
        last if !$m;
        push(@a,$m);

        my $off = $m.to +1;
#        $m.from += $offset; # XXX - should be writable, but isnt.
#        $m.to   += $offset; # XXX - should be writable, but isnt.

        last if $s eq '';

        $offset += $off;
        $s = substr($s,$off);
      }
      my $match;
      if !@a {
        $match = $new_Match(?0, undef, undef, "", [], ());
      } else {
        my $from = @a[0].from;
        my $to   = @a[-1].to + $offset; # @a[-1].to; # XXX
        my $str  = substr($string,$from,$to-$from+1);
        $match = $new_Match(?1, $from, $to, $str, @a, ());
      }
      $/ := $match;
    };
    # Here is an old JS version of :global.  Fyi.
    #      var regexp = new RegExp(pattern,flags + "g");
    #      var match  = regexp.exec(string);
    #      if (!match) {
    #        return mkMatch(false, null,null, null, null,null);
    #      }
    #      var g_from = match.index;
    #      var g_array = [];
    #      var g_to;
    #      while (match) {
    #        var m = unpack_match(match);
    #        g_array.push(m);
    #        g_to = regexp.lastIndex - 1;
    #        match = regexp.exec(string);
    #      }
    #      var g_str = string.substring(g_from,g_to+1);
    #      var m = mkMatch(true, g_from, g_to, g_str, g_array, null);
    #      return m;

  } else {
    $matcher = sub ($string) {   # XXX - Are things getting boxed?
      my $ret = JS::inline('(
        function (pattern,flags,string) {

          var mkMatch = function (ok, from, to, s, a, h) {
            return new PIL2JS.Match(ok, from, to, s, a, h);
          };
          var unpack_match = function (match) {
            var from = match.index;
            var str  = match[0];
            var to   = from + str.length -1;
            var arr  = [];
            for(var i = 1; i < match.length; i++) {
              var s = match[i];
              var tf = s != null;
              if (!tf) { s = "" }
              arr.push( mkMatch(tf, null,null, s, null,null));
            }
            var m = mkMatch(true, from, to, str, arr, null);
            return m;
          };

          var regexp = new RegExp(pattern,flags);
          var match  = regexp.exec(string);
          if (!match) {
            return mkMatch(false, null,null, null, null,null);
          }
          var m = unpack_match(match);
          return m;
        }
      )')(~$pattern,$flags,$string);
      $/ := $ret;
    };
  }

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
