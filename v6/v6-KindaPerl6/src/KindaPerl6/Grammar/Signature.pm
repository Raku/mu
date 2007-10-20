
use v6-alpha;

grammar KindaPerl6::Grammar {

    # has $.is_longname; ???
    token sig_named_only       { ':' { return 1 } | { return 0 } }
    token sig_optional         { '?' { return 1 } | { return 0 } }
    token sig_slurpy           { '*' { return 1 } | { return 0 } }
    token sig_multidimensional { '@' { return 1 } | { return 0 } }
    token sig_rw               { <?ws> 'is' <?ws> 'rw'   { return 1 } | { return 0 } }
    token sig_copy             { <?ws> 'is' <?ws> 'copy' { return 1 } | { return 0 } }

    token exp_sig_item {
        |   <sig_named_only> <sig_slurpy> <sig_multidimensional> 
            <pair>  
            <sig_optional> 
            <sig_rw> <sig_copy>   # XXX no order !!!
            { return ::Lit::SigArgument( 
                    key           => ($$<pair>)[0], 
                    value         => ($$<pair>)[1],
                    is_named_only => $$<sig_named_only>,
                    is_optional   => $$<sig_optional>,
                    is_slurpy     => $$<sig_slurpy>,
                    is_multidimensional => $$<sig_multidimensional>,
                    is_rw         => $$<sig_rw>,
                    is_copy       => $$<sig_copy>,
                ) }
        |   <sig_named_only> <sig_slurpy> <sig_multidimensional> 
            <exp>     # XXX
            <sig_optional>
            <sig_rw> <sig_copy>   # XXX no order !!!
            { return ::Lit::SigArgument( 
                    key           => $$<exp>, 
                    value         => undef,
                    is_named_only => $$<sig_named_only>,
                    is_optional   => $$<sig_optional>,
                    is_slurpy     => $$<sig_slurpy>,
                    is_multidimensional => $$<sig_multidimensional>,
                    is_rw         => $$<sig_rw>,
                    is_copy       => $$<sig_copy>,
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
        # TODO - exp_seq / exp_mapping == positional / named 
        # ??? exp_sig_list
        <exp_sig_list> 
        {
            # say ' invocant: ', ($$<invocant>).perl;
            # say ' positional: ', ($$<exp_seq>).perl;
            return ::Sig( 'invocant' => $$<invocant>, 'positional' => $$<exp_sig_list>, 'named' => { } );
        }
    };


}
