
use v6-alpha;

grammar KindaPerl6::Grammar {

    token stmt_sep {
       <?opt_ws> \; <?opt_ws> | \n <?opt_ws>
    };
    token exp_stmts {
        | <exp>
            [
            |   <?stmt_sep> <exp_stmts>
                [<?stmt_sep> | <?opt_ws>]
                { return [ $$<exp>, @( $$<exp_stmts> ) ] }
            |   [<?stmt_sep> | <?opt_ws>]
                { return [ $$<exp> ] }
            ]
        | { return [] }
    };
    token exp_stmts2 {
        <exp>
            [
            |   <?stmt_sep> <exp_stmts>
                [<?stmt_sep> | <?opt_ws>]
                { return [ $$<exp>, @( $$<exp_stmts> ) ] }
            |   [<?stmt_sep> | <?opt_ws>]
                { return [ $$<exp> ] }
            ]
    };

}

