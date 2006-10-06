use strict;
use warnings;
# Compile-time Perl 5 thing, with hardcoded, autoboxed  methods

# TODO - TypedExpression base module
# TODO - ScalarExpression, HashExpression

package Pugs::Emitter::Perl6::Perl5::AnyExpression;
    use base 'Pugs::Emitter::Perl6::Perl5::Any';
    use overload (
        '""'     => sub { $_[0]->{name} },
        fallback => 1,
    );
    sub WHAT {
        $_[0]->node( 'StrExpression', $_[0] . '->WHAT' );
    }
    sub perl {
        $_[0]->str;
    }
    sub true { 
        return $_[0]->node( 'BoolExpression', $_[0] )
    }
package Pugs::Emitter::Perl6::Perl5::BoolExpression;
    use base 'Pugs::Emitter::Perl6::Perl5::AnyExpression';
    use overload (
        '""'     => sub { $_[0]->{name} },
        fallback => 1,
    );
    sub WHAT { 
        return $_[0]->node( 'str', 'Bool' );
    }
    sub str {
        return $_[0]->node( 'StrExpression', '( '. $_[0]->{name} . ' ? 1 : 0 )' );
    }
    sub true {
        $_[0];
    }
    sub not {
        return $_[0]->node( 'BoolExpression', '( '. $_[0]->{name} . ' ? 0 : 1 )' );
    }
package Pugs::Emitter::Perl6::Perl5::StrExpression;
    use base 'Pugs::Emitter::Perl6::Perl5::AnyExpression';
    use overload (
        '""'     => sub { $_[0]->{name} },
        fallback => 1,
    );
    sub WHAT { 
        return Pugs::Emitter::Perl6::Perl5::Str->new( { name => 'Str' } );
    }
    sub str {
        $_[0]
    }
    sub scalar {
        return Pugs::Emitter::Perl6::Perl5::Scalar->new( {
            name => 'bless \\' . $_[0]->perl . 
                    ", 'Pugs::Runtime::Perl6::Str'" 
        } );
    }
    sub eq {
        Pugs::Emitter::Perl6::Perl5::BoolExpression->new( 
            { name => $_[0] . " eq " . $_[1]->str } );
    }
package Pugs::Emitter::Perl6::Perl5::IntExpression;
    use base 'Pugs::Emitter::Perl6::Perl5::AnyExpression';
    use overload (
        '""'     => sub { $_[0]->{name} },
        fallback => 1,
    );
    sub WHAT { 
        return Pugs::Emitter::Perl6::Perl5::Str->new( { name => 'Int' } );
    }
    sub str {
        return Pugs::Emitter::Perl6::Perl5::StrExpression->new( { name => $_[0]->{name} } );
    }
    sub perl {
        $_[0]->str
    }
package Pugs::Emitter::Perl6::Perl5::NumExpression;
    use base 'Pugs::Emitter::Perl6::Perl5::AnyExpression';
    use overload (
        '""'     => sub { $_[0]->{name} },
        fallback => 1,
    );
    sub WHAT { 
        return Pugs::Emitter::Perl6::Perl5::Str->new( { name => 'Num' } );
    }
    sub str {
        return Pugs::Emitter::Perl6::Perl5::StrExpression->new( { name => $_[0]->{name} } );
    }
package Pugs::Emitter::Perl6::Perl5::CodeExpression;
    use base 'Pugs::Emitter::Perl6::Perl5::AnyExpression';
    use overload (
        '""'     => sub { 'sub { ' . $_[0]->{name} . ' } ' },
        fallback => 1,
    );
    sub WHAT { 
        return Pugs::Emitter::Perl6::Perl5::Str->new( { name => 'Code' } );
    }
    sub str {
        return Pugs::Emitter::Perl6::Perl5::StrExpression->new( { name => $_[0]->{name} } );
    }
package Pugs::Emitter::Perl6::Perl5::ListExpression;
    use base 'Pugs::Emitter::Perl6::Perl5::AnyExpression';
    use overload (
        '""'     => sub { $_[0]->{name} },
        fallback => 1,
    );
    sub WHAT { 
        return Pugs::Emitter::Perl6::Perl5::Str->new( { name => 'List' } );
    }
package Pugs::Emitter::Perl6::Perl5::HashExpression;
    use base 'Pugs::Emitter::Perl6::Perl5::AnyExpression';
    use overload (
        '""'     => sub { $_[0]->{name} },
        fallback => 1,
    );
    sub WHAT { 
        return Pugs::Emitter::Perl6::Perl5::Str->new( { name => 'Hash' } );
    }

1;
