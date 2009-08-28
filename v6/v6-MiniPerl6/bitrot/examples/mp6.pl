
use v6-alpha;

class Main {

    use MiniPerl6::Grammar;
    use MiniPerl6::Emitter;
    use MiniPerl6::Grammar::Regex;
    use MiniPerl6::Emitter::Token;
    
    # say "Args: ", @*ARGS;
    
    my $source := $*IN.slurp;
    #say "*** Source:";
    #say $$source;     # v6.pm bug - requires '$$'
    my $p := MiniPerl6::Grammar.parse( $$source );
    ## my $p := MiniPerl6::Grammar.exp_stmts( $$source );
    #say "*** AST:";
    say ($$p).perl;
    #say "*** Compiled:";
    #say ($$p).emit;
    say ($$p).>>emit.join( ";\n" );

}
