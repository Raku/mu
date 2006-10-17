use strict;
use warnings;
# Compile-time Perl 5 thing, with hardcoded, autoboxed  methods

# operator name mangler:
# perl -MPugs::Runtime::Common -e ' print Pugs::Runtime::Common::mangle_ident("==") '

# TODO - List, Seq, ...


package Pugs::Emitter::Perl6::Perl5::Value;
    use base 'Pugs::Emitter::Perl6::Perl5::Any';
    sub list { 
        $_[0]->node( 'Seq', [ $_[0] ] );
    }
    sub unboxed {
        return $_[0]->{name} 
            if Scalar::Util::blessed $_[0]->{name};
        $_[0];
    }
package Pugs::Emitter::Perl6::Perl5::Bool;
    use base 'Pugs::Emitter::Perl6::Perl5::Value';
    use overload (
        '""'     => sub { $_[0]->{name} ? '1' : '0' },
        fallback => 1,
    );
    sub WHAT { 
        $_[0]->node( 'str', 'Bool' );
    }
    sub str {
        $_[0]->node( 'str', ( $_[0]->{name} ? 'Bool::True' : 'Bool::False' ) );
    }
    sub int {
        $_[0]->node( 'int', ( $_[0]->{name} ? '1' : '0' ) );
    }
    sub num {
        $_[0]->node( 'num', ( $_[0]->{name} ? '1' : '0' ) );
    }
    sub perl {
        $_[0]->str  # stringifies to perl6
    }
    sub true {
        $_[0]
    }
    sub not {
        $_[0]->{name} 
        ? $_[0]->node( 'Bool', 0 ) 
        : $_[0]->node( 'Bool', 1 )
    }
    sub scalar {
        $_[0]->node( 'Scalar', '( bless \\( my $' . $_[0]->new_id . '=' . $_[0] . 
                    "), 'Pugs::Runtime::Perl6::Bool' )" );
    }
    ::unicode_sub 'infix:<==>', sub{ 
        $_[0]->int->infix_58__60__61__61__62_( $_[1] );
    };
package Pugs::Emitter::Perl6::Perl5::Str;
    use base 'Pugs::Emitter::Perl6::Perl5::Value';
    use overload (
        '""'     => sub { Pugs::Emitter::Perl6::Perl5::str( $_[0] ) },
        fallback => 1,
    );
    sub WHAT { 
        $_[0]->node( 'str', 'Str' );
    }
    sub str {
        $_[0]
    }
    sub num {
        $_[0]->node( 'num', $_[0]->{name} + 0 );
    }
    sub int {
        $_[0]->num->int;
    }
    sub true {
        $_[0]->num->true;  # XXX
    }
    sub scalar {
        $_[0]->node( 'Scalar', '( bless \\( my $' . $_[0]->new_id . '=' . $_[0] . 
                    "), 'Pugs::Runtime::Perl6::Str' )" );
    }
    sub eq {
        $_[0]->node( 'BoolExpression', $_[0] . " eq " . $_[1]->str );
    }
package Pugs::Emitter::Perl6::Perl5::Int;
    use base 'Pugs::Emitter::Perl6::Perl5::Value';
    use overload (
        '""'     => sub { Pugs::Emitter::Perl6::Perl5::int( $_[0] ) },
        fallback => 1,
    );
    sub WHAT { 
        $_[0]->node( 'str', 'Int' );
    }
    sub str {
        $_[0]->node( 'str', $_[0]->{name} );
    }
    sub true {
        $_[0]->node( 'bool', $_[0]->{name} );
    }
    sub num {
        $_[0]->node( 'num', $_[0]->{name} );
    }
    sub int {
        $_[0];
    }
    sub scalar {
        $_[0]->node( 'Scalar', '( bless \\( my $' . $_[0]->new_id . '=' . $_[0] . 
                    "), 'Pugs::Runtime::Perl6::Int' )" );
    }
    ::unicode_sub 'infix:<==>', sub{ 
        $_[0]->num->infix_58__60__61__61__62_( $_[1] );
    };
package Pugs::Emitter::Perl6::Perl5::Num;
    use base 'Pugs::Emitter::Perl6::Perl5::Value';
    use overload (
        '""'     => sub { Pugs::Emitter::Perl6::Perl5::num( $_[0] ) },
        fallback => 1,
    );
    sub WHAT { 
        $_[0]->node( 'str', 'Num' );
    }
    sub str {
        $_[0]->node( 'str', $_[0]->{name} );
    }
    sub true {
        $_[0]->node( 'bool', $_[0]->{name} );
    }
    sub num {
        $_[0];
    }
    sub int {
        $_[0]->node( 'int', int( $_[0]->{name} ) );
    }
    sub scalar {
        $_[0]->node( 'Scalar', '( bless \\( my $' . $_[0]->new_id . '=' . $_[0] . 
                    "), 'Pugs::Runtime::Perl6::Num' )" );
    }
    ::unicode_sub 'infix:<==>', sub{ 
        my $tmp = $_[1]->num;
        return $_[0]->node( 'Bool', ( $_[0] == $tmp ) )
            if ref( $tmp ) eq 'Pugs::Emitter::Perl6::Perl5::Num';
        return $_[0]->node( 'BoolExpression', $_[0] . " == " . $tmp );
    };
package Pugs::Emitter::Perl6::Perl5::Code;
    use base 'Pugs::Emitter::Perl6::Perl5::Value';
    use overload (
        '""'     => sub { 'sub { ' . $_[0]->{name} . ' } ' },
        fallback => 1,
    );
    sub WHAT { 
        $_[0]->node( 'str', 'Code' );
    }
    sub str {
        $_[0]->node( 'str', $_[0]->{name} );
    }
    sub perl {
        $_[0]->str  # TODO
    }
package Pugs::Emitter::Perl6::Perl5::Seq;
    use base 'Pugs::Emitter::Perl6::Perl5::Value';
    use overload (
        '""'     => sub { 
            '(' . join( ', ', map { $_->boxed } @{$_[0]->{name}} ) . ')' 
        },
        fallback => 1,
    );
    sub new {
        # Seq interpolates inside another Seq
        #print "Seq.new ",Data::Dumper::Dumper(\@_);
        my @self = map {
            #print "Seq.elem ",$_->WHAT->{name}," ",Data::Dumper::Dumper($_);
            $_->WHAT->{name} eq 'Seq' 
            ? @{$_->{name}}
            : $_
            } @{$_[1]->{name}};  
        return bless { name => \@self }, $_[0];
    }
    sub WHAT { 
        $_[0]->node( 'str', 'Seq' );
    }
    sub list { 
        $_[0]
    }
    sub array {
        $_[0]
    }
    sub scalar {
        $_[0]->node( 'SeqArray', $_[0]{name} )
    }
    sub hash {
        $_[0]->node( 'SeqHash', $_[0]{name} );
    }
    sub splice {
        $_[0]->array->splice
    }
    sub elems {
        $_[0]->node( 'int', scalar @{$_[0]->{name}} )
    }
    sub num {
        $_[0]->elems->num
    }
    sub int {
        $_[0]->elems
    }
package Pugs::Emitter::Perl6::Perl5::List;
    use base 'Pugs::Emitter::Perl6::Perl5::Seq';
    # TODO
package Pugs::Emitter::Perl6::Perl5::pair;
    use base 'Pugs::Emitter::Perl6::Perl5::Value';
    use overload (
        '""'     => sub { 
            '(' . join( ' => ', map { $_ } @{$_[0]->{name}} ) . ')' 
        },
        fallback => 1,
    );
    sub WHAT { 
        $_[0]->node( 'str', 'Pair' );
    }
    sub str {
        $_[0]->node( 'str', join( "\t", @{$_[0]->{name}} ) );
    }
    sub key {
        $_[0]->{name}[0];
    }
    sub value {
        $_[0]->{name}[1];
    }
    sub kv {
        $_[0]->node( 'Seq', $_[0]->{name} );
    }
    sub scalar {
        return $_[0]->node( 'Scalar', '( bless [' . $_[0] . "], 'Pugs::Runtime::Perl6::Pair' )" )
    }
    sub hash {
        return $_[0]->node( 'Hash', '( bless [' . $_[0] . "], 'Pugs::Runtime::Perl6::Hash' )" )
    }
    sub array {
        return $_[0]->scalar->array
    }
    sub list {
        return $_[0]->scalar->list
    }

sub isa { 
    die "TODO";
    my $self = $_[0];
    return $self->other_get( $_[1] ) . ' eq ' . "'Pair'";  # hardcoded 
}

    
sub defined {
    die "TODO";
}

sub elems {
    die "TODO";
    return Pugs::Emitter::Perl6::Perl5::Scalar->new( {
        name => '1'
    } );
}

sub _123__125_ {
    die "TODO";
    # .{}
    my $self = $_[0];
    my $other = $self->other_get( $_[1] );
    return $_[0] unless $other;  # TODO
    return $self->dollar_name . '{' . $other . '}';
}

package Pugs::Emitter::Perl6::Perl5::Perl5Range;
    use strict;
    use warnings;
    use base 'Pugs::Emitter::Perl6::Perl5::Value';
    use overload (
        '""'     => sub { 
            '(' . join( ' .. ', @{$_[0]->{name}} ) . ')' 
        },
        fallback => 1,
    );

    sub WHAT { 
        $_[0]->node( 'str', 'Range' );
    }
    
1;
