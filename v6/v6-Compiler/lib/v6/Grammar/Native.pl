
use v6-alpha;

grammar v6::Grammar::Native;

use v6::AST::Native;

=for CapInternals.hs #361

-- | Unboxed or native values. They have themselves as their .id.
type ValNative = Native
data Native
    = NBit  !NativeBit     -- ^ 0
    | NInt  !NativeInt     -- ^ -3
    | NUint !NativeInt     -- ^ 7
    | NBuf  !NativeBuf     -- ^ (a raw chunk of ints or uints)
    | NNum  !NativeNum     -- ^ 4.2
    | NCplx !NativeComplex -- ^ (45 - 9i)
    | NStr  !NativeStr     -- ^ 'aloha'
    | NBool !NativeBool    -- ^ True (same underlying storage as NBit + True/False)

Int, Buf, Cplx are not parsed here - these are 'higher level' constructs:

  -3 is parsed as '-','3'

=cut

token bit {
    0 | 1
    { return $/.as(v6::AST::NBit) }
}

# from grammar_rules.pg integer() and number()
token uint {
    | 0 [ b <[01]>+           [ _ <[01]>+ ]*
        | o <[0..7]>+         [ _ <[0..7]>+ ]*
        | x <[0..9a..fA..F]>+ [ _ <[0..9a..fA..F]>+ ]*
        | d \d+               [ _ \d+]*
        ]
    | \d+[_\d+]*
    { return $/.as(v6::AST::NUint) }
}

token num {
    \d+[_\d+]* [ \. \d+[_\d+]* [ <[Ee]> <[+\-]>? \d+ ]? ]
    { return $/.as(v6::AST::NNum) }
}

token str {
    ' [ \\' | <!'> ]* '
    { return $/.as(v6::AST::NStr) }
}

token bool {
    True | False
    { return $/.as(v6::AST::NBool) }
}
