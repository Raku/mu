use strict;
use warnings;
# Compile-time Perl 5 thing, with hardcoded, autoboxed  methods

package Pugs::Emitter::Perl6::Perl5::Value;
    use base 'Pugs::Emitter::Perl6::Perl5::Any';
package Pugs::Emitter::Perl6::Perl5::Bool;
    use base 'Pugs::Emitter::Perl6::Perl5::Value';
    sub WHAT { 
        return "'Bool'";  # hardcoded 
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
        return "'Str'";  # hardcoded 
    }
    sub str {
        # TODO
    }
    sub perl {
        "'" . $_[0]->{value} . "'"
    }
    sub scalar {
        return Pugs::Emitter::Perl6::Perl5::Perl5Scalar->new( {
            name => 'bless \\' . $_[0]->perl . 
                    ", 'Pugs::Runtime::Perl6::Str'" 
    } );
}

1;
