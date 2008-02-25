        grammar MyC;

        token def {
            <type> <?ws> <var_list> <?ws>? ';'
        }

        token type { int | float | double | char }

        token var_list {
            <ident>**{1} <?ws>? [ ',' <?ws>? <ident> ]*
        }

        grammar MyVB;

        token def {
            'Dim' <?ws> <MyC.var_list>
            [ <?ws> 'As' <?ws> <MyC.type> ]? <?ws>? ';'
        }
    };
