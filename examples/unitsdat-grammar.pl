grammar UnitsDat;
# This is a grammar for the units(1) units.dat database
# TODO: parse nonlinear unit definitions

my Str @units;
my Str @prefixes;
my Hash of Num %unitsdef;
my Str @fund_units;
my Str @fund_unitless;

rule TOP {
    [ <?fundamental_unit> | <?unit> | <?prefix> ]?
    <?comment>?
}

# Multiplication is implied by whitespace in units.dat
# but * can be used also, just treat it as whitespace
token ws { \h+ | \h* '*' \h* }

token float {
    $<mantissa> := [ '-'? \d+ [ '.' \d+ ]? ]
        [ e $<exp> := [ '-'? \d+] { $<num> = $<mantissa> * 10 ** $<exp> } ]?
}

rule fraction {
    $<numerator> := [\d+] '|' $<denominator> := [\d+]
    { $<num> = $<numerator> / $<denominator> }
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
    | <fraction>     { $<num> = $<fraction><num> }
    | <float>        { $<num> = $<float><num> }
    | <builtin_func> { $<num> = $<builtin_func><num> }
}

rule simplenumber_mult {
    <basicnumber> { $<num> = $<basicnumber><num> }
    [ <?ws> <simplenumber_mult> { $<num> *= $<simplenumber_mult><num> }
    | '/'   <simplenumber_mult> { $<num> /= $<simplenumber_mult><num> }
    ]
}

rule simplenumber {
    { my Int $n }
    <simplenumber_mult> { $<num> = $<simplenumber_mult>[0]<num> }
    [ '+'  <simplenumber_mult> { $<num> += $<simplenumber_mult>[++$n]<num> }
    | '-'  <simplenumber_mult> { $<num> -= $<simplenumber_mult>[++$n]<num> }
    ]*
}

rule number_mult {
    | <simplenumber> { $<num> = $<simplenumber><num> }
    | <number_mult> { $<num> = $<number_mult>[0]<num> }
        [ <?ws> <number_mult> { $<num> *= $<number_mult>[1]<num> }
        | '/'   <number_mult> { $<num> /= $<number_mult>[1]<num> }
        ]
}

rule number {
    { my Int $n }
    | <simplenumber> { $<num> = $<simplenumber><num> }
    | '(' <number_mult> { $<num> = $<number_mult>[0]<num> }
        [ '+' <number_mult> { $<num> += $<number_mult>[++$n]<num> }
        | '-' <number_mult> { $<num> -= $<number_mult>[++$n]<num> }
        ]*
        ')'
}

token comment { '#' \N* }

token fundamental_unit {
    ^^ $<unit> := [\S+] \h+ '!' [ dimensionless { $<nodim> := True } ]?
    {
        @units.push: $<unit>;
        if($<nodim>) {
            @fund_unitless.push: $<unit>
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

token unitname {
    { $<factor> = 1 }
    [ $<prefix> := [ | @prefixes ]
        { $<factor> *= %unitsdef{$<prefix>}<factor> } ]*
    $<name> := [ <!before \S+ '-'> \S+ ] s?
}

token unit {
    ^^ <unitname> \h+ <unitdef>
    {
        @units.push: $<unitname><name>;
        $<unitdef><def><factor> *= $<unitname><factor>;
        %unitsdef{$<unitname><name>} = $<unitdef><def>;
    }
}

rule basicunitdef {
    { $<factor> = 1 }
    [ <number> { $<factor> *= $<number><num> }
    | $<uf> := [ | @units ] { $<ufp> = 1 } [ '^' <number> { $<ufp> = $<number><num> } ]?
        { $<def>{$<uf>} += $<ufp> }
    | '-' $<uf> := [ | @units ] { $<ufp> = 1 } [ '^' <number> { $<ufp> = $<number><num> } ]?
        { $<def>{$<uf>} -= $<ufp>; $<factor> *= -1; }
    ]
    { $<def><factor> = $<factor> }
}

rule simpleunitdef_mult {
    <basicunitdef> { $<def> = $<basicunitdef><def> }
    [ <?ws> <simpleunitdef_mult>  { $<def> = multdef($<def>, $<simpleunitdef_mult><def>, 1) }
    | '/'   <simpleunitdef_mult>  { $<def> = multdef($<def>, $<simpleunitdef_mult><def>, -1) }
    ]
}

rule simpleunitdef {
    { my Int $n }
    <simpleunitdef_mult> { $<def> = $<simpleunitdef_mult>[0]<def> }
    [ '+'  <simpleunitdef_mult> { $<def> = adddef($<def>, $<simpleunitdef_mult>[++$n]<def>, 1) }
    | '-'  <simpleunitdef_mult> { $<def> = adddef($<def>, $<simpleunitdef_mult>[++$n]<def>, -1) }
    ]*
}

rule unitdef_mult {
    | <simpleunitdef> { $<def> = $<simpleunitdef><def> }
    | <unitdef_mult>  { $<def> = $<unitdef_mult>[0]<def> }
        [ <?ws> <unitdef_mult>  { $<def> = multdef($<def>, $<unitdef_mult>[1]<def>, 1) }
        | '/'   <unitdef_mult>  { $<def> = multdef($<def>, $<unitdef_mult>[1]<def>, -1) }
        ]
}

rule unitdef {
    { my Int $n }
    | <simpleunitdef> { $<def> = defreduce($<simpleunitdef><def>) }
    | '(' <unitdef_mult>  { $<def> = $<unitdef_mult>[0]<def> }
        [ '+' <unitdef_mult>  { $<def> = adddef($<def>, $<unitdef_mult>[++$n]<def>, 1) }
        | '-' <unitdef_mult>  { $<def> = adddef($<def>, $<unitdef_mult>[++$n]<def>, -1) }
        ]*
        ')' { $<def> = defreduce($<def>) }
}

# reduce a unit definition to fundamental units
method defreduce(Num %def is copy --> Hash of Num) {
    until all(%def.k) eq any('factor', @fund_units, @fund_unitless) {
        for %def.kv -> (my Str $u, my Num $p) {
            next if $u eq any('factor', @fund_units, @fund_unitless);
            %def{$u}.:delete;
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
    %def1c<factor>.:delete; %def2c<factor>.:delete;
    return undef unless %def1c eqv %def2c;
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
