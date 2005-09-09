use strict;

use Perl6::MetaModel;
use Perl6::Object;

my $class_description = '-0.0.1-cpan:PUTTER';

class 'Match'.$class_description => {
    is => [ 'Perl6::Object' ],
    instance => {
        attrs => [ 
            [ '$.val_bool' => { access => 'rw' } ],
            [ '$.val_string' => { access => 'rw' } ],
            [ '$.val_array' => { access => 'rw' } ],
            [ '$.val_hash' => { access => 'rw' } ],
            [ '$.from' => { access => 'rw' } ],
            [ '$.to' => { access => 'rw' } ],
        ],
        methods => {
            'num' =>  sub { ::SELF->int },
            'int' =>  sub { Int->new( '$.unboxed' => 0 ) },
            'str' =>  sub { Str->new( '$.unboxed' => 'a match' ) },
            'bit' =>  sub { Bit->new( '$.unboxed' => 1 ) },
            'perl' => sub { ::SELF->str },
            'ref' =>  sub { ::CLASS }, 

        },
    }
};

package Match;
use overload
    'bool' => 'as_bool',
    '""'   => 'as_string',
    '@{}'  => 'as_array',
    '%{}'  => 'as_hash',
    fallback => 1
    ;

sub new_failed {
    my($cls)=@_;
    $cls->new()->set_as_failed();
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

1;
