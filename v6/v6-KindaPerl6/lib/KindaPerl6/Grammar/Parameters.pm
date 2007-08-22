
use v6-alpha;

grammar KindaPerl6::Grammar {

    sub declare_parameters( $env, $block, $sig ) {
        # declare the variables in the signature as 'my'
        #say "#declaring parameters";
        my $vars := [
                ::Var( sigil => '@', twigil => '', name => '_' ),
                $sig.invocant,
                @($sig.positional),
            ];
        for @($vars) -> $var {
            #say "#var ", $var.name;
            my $decl :=                 
                ::Decl(  
                    decl  => 'my',  
                    var   => $var,  
                    type  => '', # TODO
                );
            $env.add_lexicals( [ $decl ] );
            
            # XXX - this needs more work - it probably doesn't belong here
            ## $block := [ $decl, @($block) ];  # unshift
        };
    }

}
