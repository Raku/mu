
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
# 3i is parsed as '3', 'i' - postfix 'i' makes imaginary numbers
#
# also, tokens like 'Inf' don't check for end-of-the word, 
# and this could cause a misparse of the word 'Infinity'.
# This is fixed later by the 'longest-token' rule

# modified from grammar_rules.pg integer() and number()
token num {
    | 0 [ [ b <[01]>+           [ _ <[01]>+ ]*
          | o <[0..7]>+         [ _ <[0..7]>+ ]*
          | x <[0..9a..fA..F]>+ [ _ <[0..9a..fA..F]>+ ]*
          | d \d+               [ _ \d+]*
          | \d+[_\d+]*
            [ \. \d+[_\d+]* [ <[Ee]> <[+\-]>? \d+ ]? 
              { return $/ does v6::AST::NFloat  }
            | { return $/ does v6::AST::IFinite }
            ]
          ]
        | { return $/ does v6::AST::NBit) }
        ]
    | 1 [ \d+[_\d+]*
            [ \. \d+[_\d+]* [ <[Ee]> <[+\-]>? \d+ ]? 
              { return $/ does v6::AST::NFloat  }
            | { return $/ does v6::AST::IFinite }
            ]
        | { return $/ does v6::AST::NBit) }
        ]
    | \d+[_\d+]*
        [ \. \d+[_\d+]* [ <[Ee]> <[+\-]>? \d+ ]? 
          { return $/ does v6::AST::NFloat  }
        | { return $/ does v6::AST::IFinite }
        ]
    | \. \d+[_\d+]* [ <[Ee]> <[+\-]>? \d+ ]? 
    { return $/ does v6::AST::NFloat      }
    | Inf
    { return $/ does v6::AST::IInfinite   }
    | NaN
    { return $/ does v6::AST::INotANumber }
}

token str {
    ' ( [ \\' | <!'> ]* ) '
    { return $/[0] does v6::AST::NStr }
}

# - there is no parsetime 'bool'
#
#   <audreyt> the only way to get parsetime bool is
#   constant bool $x = True
#   or
#   my bool $x; BEGIN { $x = False } 
#
# token bool {
#    [ True | False ]
#    { return $/ does v6::AST::NBool) }
# }
