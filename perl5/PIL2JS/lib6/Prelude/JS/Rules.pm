
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

our(%modifiers_known, %modifiers_supported_p6, %modifiers_supported_p5);
our(%rx_helper_cache);
sub JS::Root::rx_helper_(%mods0, Str $pat0, Str $qo, Str $qc) is primitive {

  if !%modifiers_known.keys {
    %modifiers_known = map {;($_ => 1)},
        <perl5 Perl5 P5 i ignorecase w words g global c continue p pos
        once bytes codes graphs langs x nth ov overlap ex exhaustive
        rw keepall e each any parsetree>;
    %modifiers_supported_p6 = map {;($_ => 1)},
        <i ignorecase w words g global>;
    %modifiers_supported_p5 = map {;($_ => 1)},
        <perl5 Perl5 P5 i ignorecase g global ov overlap>;
    %rx_helper_cache{'re_x'}   = rx_core_({perl5=>1},'^(\d+)x$','/','/');
    %rx_helper_cache{'re_nth'} = rx_core_({perl5=>1},'^(\d+)(?:th|st|nd|rd)$','/','/');
  }

  my $pat = $pat0;
  my %mods = %mods0;
  my $p5 = %mods{"perl5"} || %mods{"Perl5"} || %mods{"P5"};

  my sub warning($e){warn("Warning: $e")};
  for %mods.keys -> $k {
    if %modifiers_known{$k} {
      if $p5 && !%modifiers_supported_p5{$k} {
        warning "Modifier :$k is not (yet?) supported by :perl5 regexps.";
      } elsif !$p5 && !%modifiers_supported_p6{$k} {
        warning "Modifier :$k is not yet supported by PGE/pugs.";
      }
    }
    elsif (($k.chars > 1) && (substr($k,-1,1) eq "x") # XXX - excess parens required
           && ($k ~~ %rx_helper_cache{'re_x'})) {
      my $n = 0+ ~$0; # +$0 XXX
      %mods.delete($k);
      %mods{'x'} = $n;
    }
    elsif (($k.chars > 2) && (substr($k,-2,2) eq ("th"|"st"|"nd"|"rd"))
           && ($k ~~ %rx_helper_cache{'re_nth'})) {
      my $n = 0+ ~$0; # +$0 XXX
      %mods.delete($k);
      %mods{'nth'} = $n;
    }
    else {
      my $msg = "Unknown modifier :$k will probably be ignored.";
      $msg ~= "  Perhaps you meant :i:s ?" if $k eq ("is"|"si");
      warning $msg;
    }
  }

  if !$p5 { 
    die "Perl 6 Rules are not supported.";
  }

  rx_core_(%mods,$pat,$qo,$qc);
}

sub JS::Root::rx_core_(%mods, Str $pat, Str $qo, Str $qc) is primitive {

  my $global     = %mods{'g'}  || %mods{'global'};
  my $overlap    = %mods{'ov'} || %mods{'overlap'};
  my $exhaustive = %mods{'ex'} || %mods{'exhaustive'};
  my $ignore     = %mods{'i'}  || %mods{'ignore'};
  my $nth        = %mods.exists('nth');
  my $multiline;

  my $new_Match = JS::inline('
     PIL2JS.toPIL2JSBox(function (as_b, from, to, as_s, as_a, as_h) {
        return new PIL2JS.Match(as_b, from, to, as_s, as_a, as_h);
     })');

  my $matcher;
  if $global {
    my %mods_altered = %mods;
    %mods_altered.delete('g');
    %mods_altered.delete('global');
    my $rx = rx_core_(%mods_altered,$pat,$qo,$qc);
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
        my $to   = $offset -1; # @a[-1].to; # XXX - .to is incorrect - see above
        my $str  = substr($string,$from,$to-$from+1);
        $match = $new_Match(?1, $from, $to, $str, @a, ());
      }
      $/ := $match;
    };
  } elsif $nth {
    my %mods_altered = %mods;
    %mods_altered.delete('nth');
    %mods_altered{'global'} = 1;
    my $rx = rx_core_(%mods_altered,$pat,$qo,$qc);
    my $nth_keys = %mods{'nth'};
    $matcher = sub ($string) {
      my $m = $string ~~ $rx;
      return $m if !$m;
      my @a = @$m; # XXX Error: Can't use "[object Object]" as a generic reference!
      @a = map {$_[0]}, @$m if @(@a[0]);
      unshift(@a,undef); # 1-based.
      @a[$nth_keys];
    };
  } elsif $overlap {
    my %mods_altered = %mods;
    %mods_altered.delete('ov');
    %mods_altered.delete('overlap');
    my $rx = rx_core_(%mods_altered,$pat,$qo,$qc);
    $matcher = sub ($string) {
       my $pos = 0;  my $a = []; my $prev = -1; my $m;
       while 1 {
        my $s = substr($string,$pos) // last;
        $m = $s ~~ $rx or last;
        last if !$m; # XXX - should be caught by  or last;
        my $m0 = $m[0] // $m;
        my $at = $pos + $m0.from;
        $a.push($m0) if $at > $prev; $prev = $at;
        $pos += $m.from + 1;
       }
       # 0 ?? ([|] @$a) !! $a;  # XXX - 0 should be want.Item
       $a;
    };
  } else {
    my $pattern = $pat;
    if (substr($pattern,0,2) eq '(?') {
       if substr($pattern,0,4) eq '(?i)' { # XXX - kludge - use regex
         $pattern = substr($pattern,4);
         $ignore = 1;
       }
       if substr($pattern,0,4) eq '(?m)' {
         $pattern = substr($pattern,4);
         $multiline = 1;
       }
       # TODO - x
    }
    # TODO - \A \Z \z
    my $flags   = "";
    $flags ~= "i" if $ignore;
    $flags ~= "m" if $multiline;
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

          var regexp;
          try { // Guard against malformed patterns.
            regexp = new RegExp(pattern,flags);
          } catch (e) {
            return null;
          }
          var  match  = regexp.exec(string);
          if (!match) {
            return mkMatch(false, null,null, null, null,null);
          }
          var m = unpack_match(match);
          return m;
        }
      )')(~$pattern,$flags,~$string);
      if !defined $ret {  # XXX - should really be die, not warn.
        warn "JavaScript RegExp syntax error in /$pattern/.\n";
        $ret = $new_Match(?0, undef, undef, "", [], ());
      }
      $/ := $ret;
    };
  }

  JS::inline('(function (matcher) { return new PIL2JS.Rul(matcher) })')($matcher);
}


method JS::Root::matcher (Rul $rule:) {
  JS::inline('(function (rul) { return rul.matcher })')($rule);
}
for <ok from to str subpos subnamed> -> $attr {
  Pugs::Internals::eval_perl6 "method JS::Root::$attr (Match \$match:) \{
    JS::inline('(function (match) \{ return match.$attr \})')(\$match);
  \}";
}

# Needs PIL2 and MMD to be done without hacks
sub PIL2JS::Internals::Hacks::postcircumfix_for_match_objects (
  Match $match, Int *@idxs
) is primitive {
  $match.subpos[@idxs];
}

sub PIL2JS::Internals::Hacks::init_match_postcircumfix_method () is primitive {
  JS::inline('(function () {
    PIL2JS.addmethod(
      _3aMain_3a_3aMatch,
      "postcircumfix:[]",
      _26PIL2JS_3a_3aInternals_3a_3aHacks_3a_3apostcircumfix_for_match_objects
    );
  })')();
}

# $0 etc. are aliases for $/[0] etc.

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
