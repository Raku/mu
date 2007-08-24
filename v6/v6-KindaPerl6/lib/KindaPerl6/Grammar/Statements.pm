
use v6-alpha;

grammar KindaPerl6::Grammar {

    token exp_stmts {
        | <exp>
            [
            |   <?opt_ws> \; <?opt_ws> <exp_stmts>
                <?opt_ws> [ \; <?opt_ws> | <''> ]
                { return [ $$<exp>, @( $$<exp_stmts> ) ] }
            |   <?opt_ws> [ \; <?opt_ws> | <''> ]
                { return [ $$<exp> ] }
            ]
        | { return [] }
    };
    token exp_stmts2 {
        <exp>
            [
            |   <?opt_ws> \; <?opt_ws> <exp_stmts>
                <?opt_ws> [ \; <?opt_ws> | <''> ]
                { return [ $$<exp>, @( $$<exp_stmts> ) ] }
            |   <?opt_ws> [ \; <?opt_ws> | <''> ]
                { return [ $$<exp> ] }
            ]
    };

}

