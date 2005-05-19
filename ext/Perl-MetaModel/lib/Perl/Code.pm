
class Perl::Code;

# from src/Pugs/AST/Internals.hs (see data VCode)

has Bool $.isMulti;

has Str $.subName;

# SubType is SubMethod | SubRoutine | SubBlock | SubPrim
has SubType $.subType;

# ??
#  has Perl::Pad $.subPad;

has Str $.subAssoc;

has Perl::Param @.subParams;

has Perl::Binding @.subBindings;

has Perl::SlurpLimit $.subSlurpLimit;

has Perl::Type $.subReturns;

has Bool $.subLValue;

has Code $.subBody;

method Show($self:) {
    ...
}

method Eq($self: $other) {
    ...
}

# deriving Ord, Typeable?
