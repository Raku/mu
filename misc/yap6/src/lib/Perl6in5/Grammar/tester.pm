package Perl6in5::Grammar;
use Perl6in5::Grammar;

rule program {
        star( -( stmt - ';' ) ) - eoi
};

# terminal
rule bareInt {
        match( qr|^(\d+)| )
};

# terminal
rule identifier {
        '$' . match( qr|^([A-Za-z_]\w*)| )
};

# leading terminals then expression.
rule stmt {
        ( lit( "say" ).p6ws | identifier - '=' ) - expr
        . to { print "handling stmt"; @_ }
};

# all exprs are terms. ow() is "optional wrap"
# row() is "nested optional wrap"
rule expr {
        term - star( ( '+' | '-' ) - term )
};

# all terms are factors
rule term {
        factor - star( ( '*' | '/' ) - factor )
};

# all factors are bases
rule factor {
        base - opt( lit( '**' ) - factor )
};

# all bases are either bareInts or identifiers or wrapped expr.
# This rule could have been called "BareTerminal"
rule base {
        bareInt | identifier | w( '()', expr )
};