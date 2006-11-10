use v6-alpha;
use MiniPerl6::Grammar;
use MiniPerl6::Emitter;
use MiniPerl6::Grammar::Regex;
use MiniPerl6::Emitter::Token;

# say "Args: ", @*ARGS;

my $source := $*IN.slurp;
#say "*** Source:";
#say $$source;     # v6.pm bug
my $p := MiniPerl6::Grammar.exp_stmts( $$source );
#say "*** AST:";
say ($$p).perl;
#say "*** Compiled:";
say ($$p).>>emit.join( ";\n" );
