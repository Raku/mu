
# a variable context
package Perl::Cxt;

# from src/Pugs/Types.hs

# I can't think of a simple way to express the junctive, higher order
# nature of the Cxt type.  The below is expressive enough to do the
# same thing, but the conversion is a bit "lossy".
#
# has $.cxt enum« CxtVoid CxtItem CxtSlurpy »;
# has Perl::Type $.type;  # for slurpy/item types

# On the other hand, we could actually make it a real junctive type.

type CxtVoid;  # is that enough?

# here, I think we have the same problem as with Set; how do you
# specify a higher order type?
type CxtItem ( Perl::Type );
type CxtSlurpy ( Perl::Type );

type Perl::Cxt is CxtVoid | CxtItem | CxtSlurpy;

# in fact, maybe the following syntax could auto-define the extra
# types for 1 to 1 correspondance!  :)
#
#  type Perl::Cxt is CxtVoid
#                  | CxtItem (Perl::Type)
#                  | CxtSlurpy (Perl::Type) );

