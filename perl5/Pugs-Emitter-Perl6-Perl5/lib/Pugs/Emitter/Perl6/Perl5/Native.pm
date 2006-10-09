use strict;
use warnings;
# Compile-time Perl 5 thing, with hardcoded, autoboxed  methods

# operator name mangler:
# perl Runtime::Common -e ' print Pugs::Runtime::Common::mangle_ident("==") '


package Pugs::Emitter::Perl6::Perl5::Native;
    use base 'Pugs::Emitter::Perl6::Perl5::Any';
    sub boxed {
        $_[0]->scalar
    }
package Pugs::Emitter::Perl6::Perl5::bool;
    use base 'Pugs::Emitter::Perl6::Perl5::Native';
    use overload (
        '""'     => sub { $_[0]->{name} ? '1' : '0' },
        fallback => 1,
    );
    sub WHAT { 
        return Pugs::Emitter::Perl6::Perl5::str->new( { name => 'Bool' } );
    }
    sub str {
        return Pugs::Emitter::Perl6::Perl5::str->new( 
            { name => ( $_[0]->{name} ? 'Bool::True' : 'Bool::False' ) } );
    }
    sub int {
        return Pugs::Emitter::Perl6::Perl5::int->new( 
            { name => ( $_[0]->{name} ? '1' : '0' ) } );
    }
    sub num {
        return Pugs::Emitter::Perl6::Perl5::num->new( 
            { name => ( $_[0]->{name} ? '1' : '0' ) } );
    }
    sub true {
        $_[0]
    }
    sub not {
        $_[0]->{name} 
        ? Pugs::Emitter::Perl6::Perl5::bool->new( { name => 0 } ) 
        : Pugs::Emitter::Perl6::Perl5::bool->new( { name => 1 } )
    }
    sub scalar {
        $_[0]->node( 'Scalar', '( bless \\( my $' . $_[0]->new_id . '=' . $_[0] . 
                    "), 'Pugs::Runtime::Perl6::Bool' )" );
    }
    sub _61__61_ {  # ==
        $_[0]->int->_61__61_( $_[1] );
    }
package Pugs::Emitter::Perl6::Perl5::str;
    use base 'Pugs::Emitter::Perl6::Perl5::Native';
    use overload (
        '""'     => sub { 
                        my $s = $_[0]->{name};
                        my $o = "'$s'";
                        if ( $s =~ /'/ ) {
                            $o = "q!$s!";
                            if ( $s =~ /!/ ) {
                                $o = "q($s)"
                            }
                        }
                        return $o;
                    },
        fallback => 1,
    );
    sub WHAT { 
        return Pugs::Emitter::Perl6::Perl5::str->new( { name => 'Str' } );
    }
    sub str {
        $_[0];
    }
    sub scalar {
        $_[0]->node( 'Scalar', '( bless \\( my $' . $_[0]->new_id . '=' . $_[0] . 
                    "), 'Pugs::Runtime::Perl6::Str' )" );
    }
    sub eq {
        Pugs::Emitter::Perl6::Perl5::BoolExpression->new( 
            { name => $_[0] . " eq " . $_[1]->str } );
    }
package Pugs::Emitter::Perl6::Perl5::int;
    use base 'Pugs::Emitter::Perl6::Perl5::Native';
    use overload (
        '""'     => sub { $_[0]->{name} },
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
    sub int {
        $_[0]
    }
    sub num {
        $_[0]->node( 'num', $_[0]->{name} );
    }
    sub _61__61_ {  # ==
        $_[0]->num->_61__61_( $_[1] );
    }
    sub scalar {
        $_[0]->node( 'Scalar', '( bless \\( my $' . $_[0]->new_id . '=' . $_[0] . 
                    "), 'Pugs::Runtime::Perl6::Int' )" );
    }
    ::unicode_sub 'infix:<+>', sub{ 
        $_[0]->num->infix_58__60__43__62_( $_[1] );
    };
package Pugs::Emitter::Perl6::Perl5::num;
    use base 'Pugs::Emitter::Perl6::Perl5::Native';
    use overload (
        '""'     => sub { $_[0]->{name} },   # XXX Inf NaN
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
        return $_[0]->node( 'bool', ( $_[0] == $tmp ) )
            if ref( $tmp ) eq 'Pugs::Emitter::Perl6::Perl5::Num';
        return $_[0]->node( 'BoolExpression', $_[0] . " == " . $tmp );
    };
    ::unicode_sub 'infix:<+>', sub{ 
        my $tmp = $_[1]->num;
        return Pugs::Emitter::Perl6::Perl5::num->new( 
            { name => ( $_[0]->{name} + $tmp ) } )
            if ref( $tmp ) eq 'Pugs::Emitter::Perl6::Perl5::num';
        return Pugs::Emitter::Perl6::Perl5::NumExpression->new( 
            { name => $_[0] . " + " . $tmp } );
    };

1;
