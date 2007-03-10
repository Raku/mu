# Abstract super-class - for users only.
package Match;

# "Open" Match, api one.
package MatchX2;
@ISA=qw(Match);
use overload
    'bool' => 'match_boolean',
    '""'   => 'match_string',
    '@{}'  => 'match_array',
    '%{}'  => 'match_hash',
    ;
use strict; use warnings;

sub match_boolean {${$_[0]}->{match_boolean}}
sub match_string  {${$_[0]}->{match_string}}
sub match_array   {${$_[0]}->{match_array}}
sub match_hash    {${$_[0]}->{match_hash}}

sub from          {${$_[0]}->{match_from}}
sub to            {${$_[0]}->{match_to}}

sub new {
    my($cls)=@_;
    my $o = \(my $h = {
      match_boolean => 1,
      match_string  => "",
      match_array   => [],
      match_hash    => {},
      match_from    => undef,
      match_to      => undef
    });
    bless $o,$cls;
    #$o->match_set(1,"",[],{});
    return $o;
}
sub match_set {
    my($o,$b,$s,$a,$h,$from,$to)=@_;
    $$o->{match_boolean} = $b;
    $$o->{match_string}  = $s;
    $$o->{match_array}   = $a;
    $$o->{match_hash}    = $h;
    $$o->{match_from}    = $from;
    $$o->{match_to}      = $to;
    return $o;
}
sub match_set_as_failed {
    my($o)=@_;
    $o->match_set(0,"",[],{});
    return $o;
}


sub match_describe {
    my($o)=@_;
    my $os = "$o";
    $os = $o->match__indent_except_top($os) if $os =~ /\n/;
    my $s = $o->match__describe_name_as;
    $s .= "<".($o?"1":"0").",\"$os\",[";
    for (@{$o}) { $s .= "\n".$o->match__indent($_->match_describe())."," }
    $s .= "\n " if @{$o};
    $s .= "],{";
    for my $k (keys(%{$o})) {
        my $v = $o->{$k};
        my $vs = "";
        if(ref($v) eq 'ARRAY') {
            $vs = "[\n".$o->match__indent(join(",\n",map{
                $_->match_describe
              }@$v))."\n]";
        } else {
            $vs = $v->match_describe;
        }
        $s .= "\n  $k => " .$o->match__indent_except_top($vs)."," }
    $s .= "\n " if %{$o};
    $s .= "},";
    my($from,$to)=($o->from,$o->to);
    $from = "" if !defined $from;
    $to   = "" if !defined $to;
    $s .= "$from,$to>";
    return $s;
}
sub match__indent {my($o,$s)=@_; $s =~ s/^(?!\Z)/  /mg; $s}
sub match__indent_except_top {my($o,$s)=@_; $s =~ s/^(?<!\A)(?!\Z)/  /mg; $s}
sub match__describe_name_as {
    my($o)=@_;
    my $s = overload::StrVal($o);
    $s;
}

package MatchOne;
use base 'MatchX2';
sub match__describe_name_as {
    my($o)=@_;
    my $s = overload::StrVal($o);
    $s .= "[".$$o->{'RULE'}."]" if exists $$o->{'RULE'};
    $s;
}

1;
__END__
