module Units;

grammar UnitsGeneric {

    my Str @units;
    my Str @prefixes;
    my Hash of Num %unitsdef;
    my Str @fund_units;
    my Str @fund_unitless;

    token float {
        $<mantissa> := [ '-'? \d+ [ '.' \d+ ]? ]
        [ e $<exp> := [ '-'? \d+ ] ]?
        { $<num> = $<mantissa> * 10 ** $<exp> }
    }

    rule builtin_func {
        | sin  '(' <number> ')' { $<num> = sin $<number><num> }
        | cos  '(' <number> ')' { $<num> = cos $<number><num> }
        | tan  '(' <number> ')' { $<num> = tan $<number><num> }
        | ln   '(' <number> ')' { $<num> = log $<number><num> }
        | log  '(' <number> ')' { $<num> = log $<number><num>, 10 }
        | log2 '(' <number> ')' { $<num> = log $<number><num>, 2 }
        | exp  '(' <number> ')' { $<num> = exp $<number><num> }
        | acos '(' <number> ')' { $<num> = acos $<number><num> }
        | asin '(' <number> ')' { $<num> = asin $<number><num> }
        | atan '(' <number> ')' { $<num> = atan $<number><num> }
    }

    rule basicnumber {
        [ $<basicnumber> := <fraction>
        | $<basicnumber> := <float>
        | $<basicnumber> := <builtin_func>
        ]
        { $<num> = $<basicnumber><num> }
    }

    rule number_paren {
        '(' <number> ')'
        { $<num> = $<number><num> }
    }

    rule number_pow {
        [ $<lhs> := <basicnumber>
        | $<lhs> := <number_paren>
        ]
        <?p>
        [ $<rhs> := <basicnumber>
        | $<rhs> := <number_paren>
        | $<rhs> := <number_pow>
        ]
        { $<num> = $<lhs><num> ** $<rhs><num> }
    }

    rule number_mult {
        [ $<lhs> := <basicnumber>
        | $<lhs> := <number_paren>
        | $<lhs> := <number_pow>
        ]
        [ <?m> [ $<rhs> := <basicnumber>
                | $<rhs> := <number_paren>
                | $<rhs> := <number_pow>
                | $<rhs> := <number_mult>
                ]
            { $<num> = $<lhs><num> * $<rhs><num> }
        | '/'   [ $<rhs> := <basicnumber>
                | $<rhs> := <number_paren>
                | $<rhs> := <number_pow>
                | $<rhs> := <number_mult>
                ]
            { $<num> = $<lhs><num> / $<rhs><num> }
        ]
    }

    rule number_add {
        [ $<lhs> := <basicnumber>
        | $<lhs> := <number_paren>
        | $<lhs> := <number_pow>
        | $<lhs> := <number_mult>
        ]
        [ '+' [ $<rhs> := <basicnumber>
              | $<rhs> := <number_paren>
              | $<rhs> := <number_pow>
              | $<rhs> := <number_mult>
              | $<rhs> := <number_add>
              ]
            { $<num> = $<lhs><num> + $<rhs><num> }
        | '-' [ $<rhs> := <basicnumber>
              | $<rhs> := <number_paren>
              | $<rhs> := <number_pow>
              | $<rhs> := <number_mult>
              | $<rhs> := <number_add>
              ]
            { $<num> = $<lhs><num> - $<rhs><num> }
        ]
    }

    rule number {
        [ $<number> := <basicnumber>
        | $<number> := <number_paren>
        | $<number> := <number_pow>
        | $<number> := <number_mult>
        | $<number> := <number_add>
        ]
        { $<num> = $<number><num> }
    }

    token unitname {
        { $<factor> = 1; my Int $n = 0; }
        [ @<prefix> := [ | @prefixes ]
            { $<factor> *= %unitsdef{@<prefix>[$n++]}<factor> }
        ]*
        $<name> := [ | @units ] s?
    }

    rule basicunitdef {
        [ <number> { $<factor> = $<number><num> }
        |     <unitname> {
                    $<uf> = $<unitname><name>;
                    $<factor> = $<unitname><factor>;
                    $<ufp> = 1;
                }
                [ <?p> <number> { $<ufp> = $<number><num> } ]?
                { $<def>{$<uf>} += $<ufp> }
        | '-' <unitname> {
                    $<uf> = $<unitname><name>;
                    $<factor> = $<unitname><factor>;
                    $<ufp> = 1;
                }
                [ <?p> <number> { $<ufp> = $<number><num> } ]?
                { $<def>{$<uf>} -= $<ufp>; $<factor> *= -1; }
        ]
        { $<def><factor> = $<factor> }
    }

    rule unitdef_mult {
        [ $<lhs> := <basicunitdef>
        | $<lhs> := <unitdef_paren>
        ]
        [ <?m>  [ $<rhs> := <basicunitdef>
                | $<rhs> := <unitdef_paren>
                | $<rhs> := <unitdef_mult>
                ]
            { $<def> = multdef($<lhs><def>, $<rhs><def>, 1) }
        | '/'   [ $<rhs> := <basicunitdef>
                | $<rhs> := <unitdef_paren>
                | $<rhs> := <unitdef_mult>
                ]
            { $<def> = multdef($<lhs><def>, $<rhs><def>, -1) }
        ]
    }

    rule unitdef_add {
        [ $<lhs> := <basicunitdef>
        | $<lhs> := <unitdef_paren>
        | $<lhs> := <unitdef_mult>
        ]
        [ '+' [ $<rhs> := <basicunitdef>
              | $<rhs> := <unitdef_paren>
              | $<rhs> := <unitdef_mult>
              | $<rhs> := <unitdef_add>
              ]
            { $<def> = adddef($<lhs><def>, $<rhs><def>, 1) }
        | '-' [ $<rhs> := <basicunitdef>
              | $<rhs> := <unitdef_paren>
              | $<rhs> := <unitdef_mult>
              | $<rhs> := <unitdef_add>
              ]
            { $<def> = adddef($<lhs><def>, $<rhs><def>, -1) }
        ]
    }

    rule unitdef_paren {
        '(' <unitdef> ')'
        { $<def> = $<unitdef><def> }
    }

    rule unitdef {
        [ $<unitdef> := <basicunitdef>
        | $<unitdef> := <unitdef_paren>
        | $<unitdef> := <unitdef_mult>
        | $<unitdef> := <unitdef_add>
        ]
        { $<def> = defreduce($<unitdef><def>) }
    }

    # reduce a unit definition to fundamental units
    method defreduce(Num %def is copy --> Hash of Num) {
        until all(%def.k) eq any('factor', @fund_units, @fund_unitless) {
            for %def.kv -> (my Str $u, my Num $p) {
                next if $u eq any('factor', @fund_units, @fund_unitless);
                %def.:delete{$u};
                %def<factor> *= %unitsdef{$u}<factor> ** $p;
                for %unitsdef{$u}.kv -> (my Str $cu, my Num $cp) {
                    next if $cu eq 'factor';
                    %def{$cu} += $cp * $p;
                }
            }
        }
        return %def;
    }

    method adddef(Num %def1, Num %def2, Int $sign --> Hash of Num) {
        my Num %def1c = defreduce(%def1);
        my Num %def2c = defreduce(%def2);
        my Int $f1 = %def1c<factor>;
        my Int $f2 = %def2c<factor>;
        %def1c.:delete<factor>; %def2c.:delete<factor>;
        if %def1c !eqv %def2c {
            warn "Can't add incompatible units: { %def1c } : { %def2c }\n";
            return undef;
        }
        my Num %def = %def1c;
        %def<factor> = $f1 + $sign * $f2;
        return %def;
    }

    method multdef(Num %def1, Num %def2, Int $sign --> Hash of Num) {
        my Num %def = %def1;
        %def<factor> *= $sign * %def2<factor>;
        for %def2.kv -> (my Str $u, my Num $p) {
            next if $u eq 'factor';
            $def{$u} += $sign * $p;
        }
        return %def;
    }
}

grammar UnitsDat is UnitsGeneric {
    # This is a grammar for the units(1) units.dat database
    # TODO: parse nonlinear unit definitions

    rule TOP {
        [ <?fundamental_unit> | <?unit> | <?prefix> ]?
        <?comment>?
    }
    
    # Multiplication is implied by whitespace in units.dat
    # but * can be used also, just treat it as whitespace
    token ws { \h+ | \h* '*' \h* }
    token m := &ws;

    # powers are done with ^
    token p { '^' }

    token comment { '#' \N* }

    # funny 1|2 fraction syntax
    rule fraction {
        $<numerator> := [\d+] '|' $<denominator> := [\d+]
        { $<num> = $<numerator> / $<denominator> }
    }

    token fundamental_unit {
        ^^ $<unit> := [\S+] \h+ '!' [ dimensionless { $<nodim> := True } ]?
        {
            @units.push: $<unit>;
            if($<nodim>) {
                @fund_unitless.push: $<unit>;
            } else {
                @fund_units.push: $<unit>;
            }
        }
    }

    token prefix {
        ^^ $<name> := [\S+] '-' \h+ <number>
        {
            @prefixes.push: $<name>;
            %unitsdef{$<name>}<factor> = $<number><num>;
        }
    }

    token unit {
        ^^ $<name> := [ <+[\S]-[-]>+ ] \h+ <unitdef>
        {
            @units.push: $<name>;
            %unitsdef{$<name>} = $<unitdef><def>;
        }
    }
}

grammar UnitsPerl is UnitsGeneric {
    # This is envisioned as a grammar to parse unit specifications
    # in Perl source code e.g. $foo.:as<m/s**2> (conjectured syntax)
    rule TOP { <unitdef> }

    # Use normal perl operators for multiply and power
    token m { '*' }
    token p { '**' }

    # Perl doesn't have any special fraction syntax
    rule fraction { <fail> }
}
