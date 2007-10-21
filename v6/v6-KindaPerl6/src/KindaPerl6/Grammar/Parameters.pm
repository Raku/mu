
use v6-alpha;

grammar KindaPerl6::Grammar {

    sub declare_parameters( $env, $block, $sig ) {       # XXX - $block is unused
        # declare the variables in the signature as 'my'
        # TODO - declare the named parameters
        #say "#declaring parameters";
        my $vars := [
                ::Var( sigil => '@', twigil => '', name => '_', namespace => [ ] ),
                $sig.invocant,
                @($sig.positional).>>key,
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

    token exp_parameter_item {
        |   <pair>  
            { return ::Lit::NamedArgument( 
                    key           => ($$<pair>)[0], 
                    value         => ($$<pair>)[1],
                ) }
        |   <exp>  { return $$<exp> }
    }

    token exp_parameter_list {
        |   <exp_parameter_item> 
            [
            |   <?opt_ws> \, <?opt_ws> <exp_parameter_list> 
                { return [ $$<exp_parameter_item>, @( $$<exp_parameter_list> ) ] }
            |   <?opt_ws> [ \, <?opt_ws> | '' ]
                { return [ $$<exp_parameter_item> ] }
            ]
        |
            { return [ ] }
    };



}
