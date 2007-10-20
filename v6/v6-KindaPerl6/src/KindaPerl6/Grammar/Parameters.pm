
use v6-alpha;

grammar KindaPerl6::Grammar {

    sub declare_parameters( $env, $block, $sig ) {       # XXX - $block is unused
        # declare the variables in the signature as 'my'
        # TODO - declare the named parameters
        #say "#declaring parameters";
        my $vars := [
                ::Var( sigil => '@', twigil => '', name => '_', namespace => [ ] ),
                $sig.invocant,
                @($sig.positional),
            ];
        my $decl;
        my $var;
        for @($vars) -> $var {
            if $var.isa( 'Var' ) {
                #say "#var ", $var.name;
                push @($decl),                 
                    ::Decl(  
                        decl  => 'my',  
                        var   => $var,  
                        type  => '', # TODO
                    );
            }
        };
        $env.add_lexicals( $decl );
    }

    token exp_parameter_list {
        # TODO - put back named params !!!
        |   <exp> 
            [
            |   <?opt_ws> \, <?opt_ws> <exp_parameter_list> 
                { return [ $$<exp>, @( $$<exp_parameter_list> ) ] }
            |   <?opt_ws> [ \, <?opt_ws> | '' ]
                { return [ $$<exp> ] }
            ]
        |
            { return [ ] }
    };



}
