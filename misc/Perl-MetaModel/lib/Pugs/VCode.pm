
class Perl::VCode;

# from src/Pugs/AST/Internals.hs (see data VCode)

has Bool $.isMulti;

has Str $.subName;

# SubType is SubMethod | SubRoutine | SubBlock | SubPrim
enum SubType «SubMethod SubRoutine SubBlock SubPrim»;
has SubType $.subType;

# ?? do we need this?
#  has Perl::Pad $.subPad;

has Str $.subAssoc;

has Array of Perl::Param @.subParams;

has Array of Perl::Binding @.subBindings;

# so, a SlurpLimit is a tuple of Int, Perl::Exp.  Maybe this needs to
# be an Array is shape() or another object or something like that.
type SlurpLimit is ( Int, Perl::Exp );

has SlurpLimit $.subSlurpLimit;

has Perl::Type $.subReturns;

has Bool $.subLValue;

has Code $.subBody;

does Show;
does Eq;
does Ord;
does Typeable;
