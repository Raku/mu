
class Perl::Type;

# from src/Pugs/Types.hs
does Eq;
does Ord;
does Typeable;

# which is better, inheritance or a junctive type?  hmm..
# type Perl::Type is Perl::Type::Regular | Perl::Type::Junctive;

class Perl::Type::Regular isa Perl::Type;
has Str $.type;

class Perl::Type::Junctive isa Perl::Type;

# junctive types can only be disjunctions and conjunctions, so
# sub-type JuncType
has Perl::JuncType $.junc_type where { $_ =:= one«JAny JAll» };

has Perl::Type $.one;
has Perl::Type $.two;
