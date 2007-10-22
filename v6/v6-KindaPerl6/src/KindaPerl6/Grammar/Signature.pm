
use v6-alpha;

grammar KindaPerl6::Grammar {

    # has $.is_longname; ???
    token sig_type {
        |   [ <'::'> | <''> ]  <full_ident> <?ws>  { return $$<full_ident> }
        |   <''>                                   { return '' }
    }
    token sig_default_value {
        |   <?opt_ws> '=' <?opt_ws> <exp> <?opt_ws> { return { has_default => 1, default => $$<exp>, } }
        |   <''>                                     { return { has_default => 0, default => ::Val::Undef( ), } }
    }
    token sig_named_only       { ':' { return 1 } | { return 0 } }
    token sig_optional         { '?' { return 1 } | { return 0 } }
    token sig_slurpy           { '*' { return 1 } | { return 0 } }
    token sig_multidimensional { '@' { return 1 } | { return 0 } }
    token sig_rw               { <?ws> 'is' <?ws> 'rw'   { return 1 } | { return 0 } }
    token sig_copy             { <?ws> 'is' <?ws> 'copy' { return 1 } | { return 0 } }

    token exp_sig_item {
            <sig_type>
            <sig_named_only> <sig_slurpy> <sig_multidimensional> 
            
            <sigil> <ident>

            <sig_optional>
            
            <sig_default_value>
            <sig_rw> <sig_copy>   # XXX no order !!!
            
            { return ::Lit::SigArgument( 
                    key           => ::Var(
                            sigil     => ~$<sigil>,
                            twigil    => '',
                            name      => ~$<ident>,
                            namespace => [],
                        ), 
                    value         => ($$<sig_default_value>){'default'},
                    type          => $$<sig_type>,
                    has_default   => ::Val::Bit( bit => ($$<sig_default_value>){'has_default'} ),
                    is_named_only => ::Val::Bit( bit => $$<sig_named_only>  ),
                    is_optional   => ::Val::Bit( bit => $$<sig_optional>    ),
                    is_slurpy     => ::Val::Bit( bit => $$<sig_slurpy>      ),
                    is_multidimensional => 
                                     ::Val::Bit( bit => $$<sig_multidimensional> ),
                    is_rw         => ::Val::Bit( bit => $$<sig_rw>          ),
                    is_copy       => ::Val::Bit( bit => $$<sig_copy>        ),
                ) }
    }

    token exp_sig_list {
        |   <exp_sig_item> 
            [
            |   <?opt_ws> \, <?opt_ws> <exp_sig_list> 
                { return [ $$<exp_sig_item>, @( $$<exp_sig_list> ) ] }
            |   <?opt_ws> [ \, <?opt_ws> | '' ]
                { return [ $$<exp_sig_item> ] }
            ]
        |
            { return [ ] }
    };

    token sig {
        <invocant>
        <?opt_ws> 
        <exp_sig_list> 
        {
            # say ' invocant: ', ($$<invocant>).perl;
            # say ' positional: ', ($$<exp_seq>).perl;
            return ::Sig( 'invocant' => $$<invocant>, 'positional' => $$<exp_sig_list>, );
        }
    };


}
