
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
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl5, JSON!-}

-- | L<S06/"Undefined types">
data ValUndef
    = UndefUnit                -- ^ e.g., "my $x" with out further assignment
    | UndefWhatever
    | UndefFailure    !ObjId

type NativeBit   = Bool
type NativeBool  = Bool

data Sign
    = SPositive
    | SNegative

data NativeInt
    = IFinite      !Integer
    | IInfinite    !Sign
    | INotANumber

data NativeNum
    = NRational  !Rational
    | NFloat     !Float

type NativeStr = Str

-- Inf or NaN if either part is Inf or NaN.
data NativeComplex = MkComplex
    { real      :: !NNum
    , imaginary :: !NNum
    }

=cut

# Int, Buf, Cplx are not parsed at the token level
#
# for example, -3 is parsed as '-','3' because there are some operators with
# tighter precedence than '-'.
#
# also, tokens like 'Inf' don't check for end-of-the word, 
# and this could cause a misparse of the word 'Infinity'.
# This is fixed later by the 'longest-token' rule

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
    { return $/.as(v6::AST::IFinite) }
    | \d+[_\d+]*
    { return $/.as(v6::AST::IFinite) }
    | Inf
    { return $/.as(v6::AST::IInfinite) }
    | NaN
    { return $/.as(v6::AST::INotANumber) }
}

token num {
    | \d+[_\d+]* [ \. \d+[_\d+]* [ <[Ee]> <[+\-]>? \d+ ]? ]
    { return $/.as(v6::AST::NFloat) }
    | \. \d+[_\d+]* [ <[Ee]> <[+\-]>? \d+ ]? 
    { return $/.as(v6::AST::NFloat) }
}

token str {
    ' [ \\' | <!'> ]* '
    { return $/.as(v6::AST::NStr) }
}

token bool {
    True | False
    { return $/.as(v6::AST::NBool) }
}
