
class Perl::Exp;

# from src/Pugs/Internals.hs, `data Exp'

# this one doesn't translate so trivially.

type Noop;

# hmm, there's a lot.  And we probably don't need it all for the
# meta-model, except perhaps to express sub-types, default values,
# etc.
enum ExpType «Noop App Syn Cxt Pos Pad Sym Stmts Prim Val Var NonTerm»;

has ExpType $.type;
