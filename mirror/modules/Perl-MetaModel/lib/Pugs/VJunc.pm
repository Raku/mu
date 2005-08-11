
class Perl::VJunc;

# from src/Pugs/AST/Internals.hs, line starting `data VJunc'

has Perl::JuncType $.juncType;

has Set of Val $.juncDup;
has Set of Val $.juncSet;

does Eq;
does Ord;
