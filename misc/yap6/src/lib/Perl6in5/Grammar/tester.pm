package Perl6in5::Grammar;
use Perl6in5::Grammar;

rule program {
        star( -( ( stmt 
        . to { return {%{$_[0]}, 
            ast=>[[],[tail($_[0]->{ast}),['stmt',head($_[0]->{ast})]]] } } ) - ';' ) ) - eoi
};

# terminal
rule bareInt {
        match( qr|^(\d+)| )
        . to { return {%{$_[0]}, 
            ast=>[[],[tail($_[0]->{ast}),['bareInt',head($_[0]->{ast})]]] } }
};

# terminal
rule identifier {
        ( '$' . match( qr|^([A-Za-z_]\w*)| ) )
        . to { return {%{$_[0]}, 
            ast=>[[],[tail($_[0]->{ast}),['identifier',head($_[0]->{ast})]]] } }
};

# leading terminals then expression.
rule stmt {
        ( lit( "say" ).p6ws | identifier - '=' ) - expr
};

# all exprs are terms.
rule expr {
        term - star( ( '+' | '-' ) - term )
        . to { return {%{$_[0]}, 
            ast=>[[],[tail($_[0]->{ast}),['expr',head($_[0]->{ast})]]] } }
};

# all terms are factors
rule term {
        factor - star( ( '*' | '/' ) - factor )
        . to { return {%{$_[0]}, 
            ast=>[[],[tail($_[0]->{ast}),['term',head($_[0]->{ast})]]] } }
};

# all factors are bases
rule factor {
        base - opt( lit( '**' ) - factor )
        . to { return {%{$_[0]}, 
            ast=>[[],[tail($_[0]->{ast}),['factor',head($_[0]->{ast})]]] } }
};

# all bases are either bareInts or identifiers or wrapped expr.
# This rule could have been called "BareTerminal"
rule base {
        ( bareInt | identifier | w( '()', expr ) )
        . to { return {%{$_[0]}, 
            ast=>[[],[tail($_[0]->{ast}),['base',head($_[0]->{ast})]]] } }
};