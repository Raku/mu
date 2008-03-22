#line 2 Match.pm
# This code was originally taken from yet_another_regex_engine/Regexp_ModuleA.pm,
# r20138.
{ package Match;
  sub new {
    my($cls)=@_;
    my $h = {
      from    => undef,
      to      => undef,
      result  => undef,
      bool => 1,
      str  => "",
      array   => [],
      hash    => {},
      };
    my $o = \$h;
    bless $o,$cls;
  }
  sub from { shift->{from} }
  sub to { shift->{from} }

  sub match_string { shift->{str} }
  sub match_array { shift->{array}||[] }
  sub match_hash { shift->{hash} }
  sub match_boolean { shift->{bool} }
  sub match_value { undef }
  sub match_describe {
    my($o,$verbosity)=@_;
    $verbosity ||= 0;
    my $vp = $verbosity;
    my $os = $o->match_string;
    if($verbosity > 1) {
      $os = $o->match__indent_except_top($os) if $os =~ /\n/;
    } else {
      $os =~ s/\n/\\n/g;
      $os =~ s/\t/\\t/g;
      if(length($os) > 60) {
        $os = substr($os,0,30).' ... '.substr($os,-30);
      }
    }
    my $s = $o->match__describe_name_as($verbosity);
    $s .= "<".($o->match_boolean?"1":"0").",\"$os\",[";
    for my $v (@{$o->match_array}) {
      my $vs = "";
      if(ref($v) eq 'ARRAY') {
        $vs = "[\n".$o->match__indent(join(",\n",map{
          $_->match_describe($vp)
          }@$v))."\n]";
      } else {
        $vs = $v->match_describe($vp);
      }
      $s .= "\n".$o->match__indent($vs).",";
    }
    $s .= "\n " if @{$o->match_array};
    $s .= "],{";
    for my $k (keys(%{$o->match_hash})) {
      my $v = $o->match_hash->{$k};
      my $vs = "";
      if(ref($v) eq 'ARRAY') {
        $vs = "[\n".$o->match__indent(join(",\n",map{
          $_->match_describe($vp)
          }@$v))."\n]";
      } elsif(!ref($v)) {
        $vs = $v;
      } else {
        $vs = $v->match_describe($vp);
      }
      $s .= "\n  $k => " .$o->match__indent_except_top($vs).",";
    }
    $s .= "\n " if %{$o->match_hash};
    $s .= "},";
    my($from,$to)=($o->from,$o->to);
    $from = "" if !defined $from;
    $to   = "" if !defined $to;
    $s .= "$from,$to";
    my $val = $o->match_value;
    $s .= defined $val ? ",$val" : "";
    $s .= ">";
    return $s;
  }
  sub match__indent {my($o,$s)=@_; $s =~ s/^(?!\Z)/  /mg; $s}
  sub match__indent_except_top {my($o,$s)=@_; $s =~ s/^(?<!\A)(?!\Z)/  /mg; $s}
  sub match__describe_name_as {
    my($o,$verbosity)=@_;
    return "" if not $verbosity;
    my $s = "";
    $s .= $o->{rule} if defined $o->{rule};
    $s = overload::StrVal($o).'{'.$s.'}' if $verbosity > 1;
    $s;
  }
}
