#line 1 Match.pm
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
    my($o,$verbose_p)=@_;
    my $vp = $verbose_p;
    my $os = $o->match_string;
    $os = $o->match__indent_except_top($os) if $os =~ /\n/;
    my $s = $verbose_p ? $o->match__describe_name_as : "";
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
    my($o)=@_;
    my $s = overload::StrVal($o);
    $s .= "{".$$o->{RULE}."}" if defined $$o->{RULE};
    $s;
  }
}
