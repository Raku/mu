use strict;
use warnings;
# Compile-time Perl 5 thing, with hardcoded, autoboxed  methods

package Pugs::Emitter::Perl6::Perl5::Value;
    use base 'Pugs::Emitter::Perl6::Perl5::Any';
package Pugs::Emitter::Perl6::Perl5::Bool;
    use base 'Pugs::Emitter::Perl6::Perl5::Value';
    sub WHAT { 
        return Pugs::Emitter::Perl6::Perl5::Str->new( { name => 'Bool' } );
    }
    sub str {
        # TODO
    }
    sub perl {
        # TODO
    }
package Pugs::Emitter::Perl6::Perl5::Str;
    use base 'Pugs::Emitter::Perl6::Perl5::Value';
    sub WHAT { 
        return Pugs::Emitter::Perl6::Perl5::Str->new( { name => 'Str' } );
    }
    sub str {
        "'" . $_[0]->{name} . "'"
    }
    sub perl {
        "'" . $_[0]->{name} . "'"
    }
    sub scalar {
        return Pugs::Emitter::Perl6::Perl5::Perl5Scalar->new( {
            name => 'bless \\' . $_[0]->perl . 
                    ", 'Pugs::Runtime::Perl6::Str'" 
        } );
    }
package Pugs::Emitter::Perl6::Perl5::Int;
    use base 'Pugs::Emitter::Perl6::Perl5::Value';
    sub WHAT { 
        return Pugs::Emitter::Perl6::Perl5::Str->new( { name => 'Int' } );
    }
    sub str {
        $_[0]->{name}
    }
    sub perl {
        $_[0]->{name}
    }
package Pugs::Emitter::Perl6::Perl5::Num;
    use base 'Pugs::Emitter::Perl6::Perl5::Value';
    sub WHAT { 
        return Pugs::Emitter::Perl6::Perl5::Str->new( { name => 'Num' } );
    }
    sub str {
        $_[0]->{name}
    }
    sub perl {
        $_[0]->{name}
    }
package Pugs::Emitter::Perl6::Perl5::Code;
    use base 'Pugs::Emitter::Perl6::Perl5::Value';
    sub WHAT { 
        return Pugs::Emitter::Perl6::Perl5::Str->new( { name => 'Code' } );
    }
    sub str {
        $_[0]->{name}
    }
    sub perl {
        $_[0]->{name}
    }

1;
