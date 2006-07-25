
use v6-alpha;

grammar v6::Grammar::Native;

use v6::AST::Native;

# See also: 'CapInternals.hs' #361 in Pugs source
# See also: 'grammar_rules.pg' in Parrot source
# 
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

token num {
    | 0 [ [ b <[01]>+           [ _ <[01]>+ ]*
          | o <[0..7]>+         [ _ <[0..7]>+ ]*
          | x <[0..9a..fA..F]>+ [ _ <[0..9a..fA..F]>+ ]*
          | d \d+               [ _ \d+]*
          | \d+[_\d+]*
            [ \. \d+[_\d+]* [ <[Ee]> <[+\-]>? \d+ ]? 
              { return $/ does v6::AST::NFloat  }
            | { return $/ does v6::AST::NUint   }
            ]
          ]
        | { return $/ does v6::AST::NBit) }
        ]
    | 1 [ \d+[_\d+]*
            [ \. \d+[_\d+]* [ <[Ee]> <[+\-]>? \d+ ]? 
              { return $/ does v6::AST::NFloat  }
            | { return $/ does v6::AST::NUint   }
            ]
        | { return $/ does v6::AST::NBit) }
        ]
    | \d+[_\d+]*
        [ \. \d+[_\d+]* [ <[Ee]> <[+\-]>? \d+ ]? 
          { return $/ does v6::AST::NFloat  }
        | { return $/ does v6::AST::NUint   }
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
