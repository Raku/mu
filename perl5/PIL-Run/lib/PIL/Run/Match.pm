use strict;

use Perl6::MetaModel;
use Perl6::Object;

my $class_description = '-0.0.1-cpan:PUGS';

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
            'int' =>  sub { Int->new( '$.unboxed' => 0+::SELF->val_string() ) },
            'str' =>  sub { Str->new( '$.unboxed' => ::SELF->val_string() ) },
            'bit' =>  sub { Bit->new( '$.unboxed' => ::SELF->val_bool() ) },
            'perl' => sub { ::SELF->str },
            'ref' =>  sub { ::CLASS }, 
	    fetch => sub {
		my($o,$idx)=@_;
		return ::SELF->val_array() if @_ < 2;
		::SELF->val_array()->[$idx];
	    },

            set_as_failed => sub {
                ::SELF->val_bool( 0 );
                ::SELF->val_string( "" );
                ::SELF->val_array( [] );
                ::SELF->val_hash( {} );
                ::SELF;
            },
            init => sub {
                ::SELF->val_bool( 1 );
                ::SELF->val_string( "" );
                ::SELF->val_array( [] );
                ::SELF->val_hash( {} );
                ::SELF;
            },

            describe => sub {
    my($o)=@_;
    my $s = ($o->val_string)."<".($o?"1":"0").",\"$o\",[";
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
            },

            _indent => sub {my($o,$s)=@_; $s =~ s/^(?!\Z)/  /mg; $s},
            _indent_except_top => sub {my($o,$s)=@_; $s =~ s/^(?<!\A)(?!\Z)/  /mg; $s},
        },
    }
};


1;
