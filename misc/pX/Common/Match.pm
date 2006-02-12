package Match;
package Match::Subrule;

package MatchX;
package MatchX::Subrule;
@ISA=qw(MatchX Match::Subrule);
package MatchX;
@ISA=qw(Match);
use overload
    'bool' => 'as_bool',
    '""'   => 'as_string',
    '@{}'  => 'as_array',
    '%{}'  => 'as_hash',
    ;

sub as_bool   {my($o)=@_;$$o->{'val_bool'}}
sub as_string {my($o)=@_;$$o->{'val_string'}}
sub as_array  {my($o)=@_;$$o->{'val_array'}}
sub as_hash   {my($o)=@_;$$o->{'val_hash'}}

sub from {my($o)=@_;$$o->{'from'}}
sub to   {my($o)=@_;$$o->{'to'}}

sub new {
    my($cls,%args)=@_;
    my $h = {};
    my $o = (bless \$h,$cls)->init();
    $$o->{'val_string'} = $args{'str'} if defined $args{'str'};
    return $o;
}
sub init {
    my($o)=@_;
    $o->set(1,"",[],{});
    return $o;
}
sub set {
    my($o,$b,$s,$a,$h,$from,$to)=@_;
    $$o->{'val_bool'}   = $b;
    $$o->{'val_string'} = $s;
    $$o->{'val_array'}  = $a;
    $$o->{'val_hash'}   = $h;
    $$o->{'from'}  = $from;
    $$o->{'to'}    = $to;
    return $o;
}
sub set_as_failed {
    my($o)=@_;
    $o->set(0,"",[],{});
    return $o;
}
sub set_str {my($o,$s)=@_;$$o->{'val_string'}=$s;}
sub describe {
    my($o)=@_;
    my $s = overload::StrVal($o)."<".($o?"1":"0").",\"$o\",[";
    for (@{$o}) { $s .= "\n".$o->_indent($_->describe())."," }
    $s .= "\n " if @{$o};
    $s .= "],{";
    for (keys(%{$o})) {
        $s .= "\n$_ => " .$o->_indent_except_top($o->describe())."," }
    $s .= "\n " if %{$o};
    $s .= "},";
    my($from,$to)=($o->from,$o->to);
    $from = "" if !defined $from;
    $to   = "" if !defined $to;
    $s .= "$from,$to>";
    return $s;
}
sub _indent {my($o,$s)=@_; $s =~ s/^(?!\Z)/  /mg; $s}
sub _indent_except_top {my($o,$s)=@_; $s =~ s/^(?<!\A)(?!\Z)/  /mg; $s}
