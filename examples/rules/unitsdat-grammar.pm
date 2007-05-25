module Units;

grammar UnitsGeneric {

    # Expects to have these attributes available:
    #
    # A list of all valid unit names
    # Str @.units;
    #
    # A list of all unit prefixes (defined in %.unitsdef)
    # Str @.prefixes
    #
    # Definitions of all derived units (and prefixes)
    # in terms of fundamental units (which do not appear)
    # Hash of Num %.unitsdef

    # Perl and units.dat use different syntax for multiplication and power
    # operators.  Those derived grammars need to supply these tokens.
    token m  { ... }
    token p  { ... }

    # Units.dat has a special fraction syntax; Perl doesn't.
    token fraction { ... }

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
        [ @<prefix> := [ | @.prefixes ]
            { $<factor> *= %.unitsdef{@<prefix>[$n++]}<factor> }
        ]*
        $<name> := [ | @.units ] s?
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
            { $<def> = $.multdef($<lhs><def>, $<rhs><def>, 1) }
        | '/'   [ $<rhs> := <basicunitdef>
                | $<rhs> := <unitdef_paren>
                | $<rhs> := <unitdef_mult>
                ]
            { $<def> = $.multdef($<lhs><def>, $<rhs><def>, -1) }
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
            { $<def> = $.adddef($<lhs><def>, $<rhs><def>, 1) }
        | '-' [ $<rhs> := <basicunitdef>
              | $<rhs> := <unitdef_paren>
              | $<rhs> := <unitdef_mult>
              | $<rhs> := <unitdef_add>
              ]
            { $<def> = $.adddef($<lhs><def>, $<rhs><def>, -1) }
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
        { $<def> = $.defreduce($<unitdef><def>) }
    }
}

grammar UnitsDat is UnitsGeneric {
    # This is a grammar for the units(1) units.dat database
    # TODO: parse nonlinear unit definitions

    rule TOP {
        [ ^^
            [ <?fundamental_unit> | <?unit> | <?prefix> ]?
            <?comment>?
        $$ ]*
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
        $<unit> := [\S+] \h+ '!' [ dimensionless { $<nodim> := True } ]?
        {
            @.units.push: $<unit>;
            if($<nodim>) {
                @.fund_unitless.push: $<unit>;
            } else {
                @.fund_units.push: $<unit>;
            }
        }
    }

    token prefix {
        $<name> := [\S+] '-' \h+ <number>
        {
            @.prefixes.push: $<name>;
            %.unitsdef{$<name>}<factor> = $<number><num>;
        }
    }

    token unit {
        $<name> := [ <+[\S]-[-]>+ ] \h+ <unitdef>
        {
            @.units.push: $<name>;
            %.unitsdef{$<name>} = $<unitdef><def>;
        }
    }
}

grammar UnitsPerl is UnitsGeneric {
    # This is envisioned as a grammar to parse unit specifications
    # in Perl source code e.g. $foo.:as<m/s**2> (conjectured syntax)
    rule TOP {
        <unitdef>
        { $<def> = $<unitdef><def> }
    }

    # Expects to have these attributes available:
    #
    # A list of all fundamental non-unitless units
    # Str @.fund_units
    #
    # A list of all fundamental unitless units
    # Str @.fund_unitless

    # Use normal perl operators for multiply and power
    token m { '*' }
    token p { '**' }

    # Perl doesn't have any special fraction syntax
    rule fraction { <fail> }
}

role GenericUnit {

    # Unit definitions
    has Str @.units;
    has Str @.prefixes;
    has Hash of Num %.unitsdef;
    has Str @.fund_units;
    has Str @.fund_unitless;

    # My unit - a unit is a hash with fundamental unit names as keys
    # and the corresponding exponent as the value, plus a 'factor' key
    # that specifies a constant factor.  E.g. a Joule is
    # { :kg :m(2) :s(-2) :factor }
    # a milliJoule is
    # { :kg :m(2) :s(-2) :factor(1e-3) }
    # and a byte is
    # { :bit :factor(8) }
    # Default unit { :factor } is 1
    has Num %.unit = { :factor };

    # is this object unitless, e.g. can it be used as the arg to cos or exp?
    method is_unitless(--> Bool) {
        return !defined %.unit || all(%.unit.k) eq any('factor', @.fund_unitless);
    }

    method add_prefix(Str $name, Num $value -->) {
        @.prefixes.push: $name;
        %.unitsdef{$name} = { :factor($value) };
    }

    method add_fund_unit(Str $name -->) {
        @.fund_units.push: $name;
        @.units.push:      $name;
    }

    method add_fund_unitless(Str $name -->) {
        @.fund_unitless.push: $name;
        @.units.push:      $name;
    }

    multi method add_unit(Str $name, Num %def -->) {
        @.units.push: $name;
        %.unitsdef{$name} = %def;
    }

    multi method add_unit(Str $name, Str $unitspec -->) {
        if ! $unitspec ~~ /<UnitsPerl>/ {
            die "Couldn't parse $unitspec\n";
        }
        @.units.push: $name;
        %.unitsdef{$name} = $<def>;
    }

    # reduce a unit definition to fundamental units
    method defreduce(Num %def is copy --> Hash of Num) {
        until all(%def.k) eq any('factor', @.fund_units, @.fund_unitless) {
            for %def.kv -> (my Str $u, my Num $p) {
                next if $u eq any('factor', @.fund_units, @.fund_unitless);
                %def.:delete{$u};
                %def<factor> *= %.unitsdef{$u}<factor> ** $p;
                for %.unitsdef{$u}.kv -> (my Str $cu, my Num $cp) {
                    next if $cu eq 'factor';
                    %def{$cu} += $cp * $p;
                }
            }
        }
        return %def;
    }

    method adddef(Num %def1, Num %def2, Int $sign --> Hash of Num) {
        my Num %def1c = $.defreduce(%def1);
        my Num %def2c = $.defreduce(%def2);
        my Int $f1 = %def1c.:delete<factor>;
        my Int $f2 = %def2c.:delete<factor>;
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

role NumUnit does GenericUnit {

    BEGIN {    #XXX is this wrong?

        # NumUnit handles most of the traditional unit stuff via units.dat.
        my $unitsdat = open "/usr/share/misc/units.dat"
            err die "You don't have a readable /usr/share/misc/units.dat: $!";
        cat =$unitsdat ~~ /<UnitsDat>/;
        $unitsdat.close;

        # It differs from the StrUnit and StrDispUnit roles in
        # its view of Unicode units such as code, graph, etc.
        # To a NumUnit, these are all fundamental units, i.e. inconvertible.
        #TODO parse unicode data tables to get language-specific defs
        $.add_fund_unit: 'code';
        $.add_unit: 'codepoint', 'code';
        $.add_fund_unit: 'graph';
        $.add_unit: 'grapheme', 'graph';

    }

    # Define:
    # $foo.:as<m/s**2>
    # 2.:as<graphs>
    multi method :as(Num $self: Str $unitspec --> NumUnit) {
        $self does NumUnit;
        if ! $unitspec ~~ /<UnitsPerl>/ {
            die "Couldn't parse $unitspec\n";
        }
        %.unit =$<def>;
        return self;
    }

    # Convert:
    # ($six_feet - $two_mm).:as<furlongs>
    # (4.:as<bits> * 2.5.:as<GHz>).:as<ns/(1500*bytes)>
    multi method :as(NumUnit $self: Str $unitspec --> NumUnit) {
        if ! $unitspec ~~ /<UnitsPerl>/ {
            die "Couldn't parse $unitspec\n";
        }
        my Num %to_unit = $<def>;
        my Num %from_unit = %.unit;
        my $from_factor = %from_unit.:delete<factor>;
        my $to_factor = %to_unit.:delete<factor>;
        if %from_unit !eqv %to_unit {
            # Check for inverse units
            my %inverse_to_unit = $.multdef({ :factor }, %to_unit, -1)
            %inverse_to_unit.:delete<factor>;
            if %from_unit eqv %inverse_to_unit {
                %to_unit = %inverse_to_unit;
                $to_factor **= -1;
            } else {
                warn "Can't convert incompatible units: { %from_unit } : { %to_unit }\n";
                return undef;
            }
        }
        %.unit = $.multdef(%from_unit, %to_unit, -1);
        %.unit<factor> = $from_factor / $to_factor;
        return self;
    }

    multi *infix:<+>(NumUnit $n1, NumUnit $n2) {
        my Num %def = $.adddef($n1.unit, $n2.unit, 1);
        #XXX how to get the right +?
        my Num $n = $n1 + $n2;
        $n does NumUnit;
        $n.unit = %def;
        return $n;
    }

    multi *infix:<->(NumUnit $n1, NumUnit $n2) {
        my Num %def = $.adddef($n1.unit, $n2.unit, -1);
        my Num $n = $n1 - $n2;
        $n does NumUnit;
        $n.unit = %def;
        return $n;
    }

    multi *infix:<*>(NumUnit $n1, NumUnit $n2) {
        my Num %def = $.multdef($n1.unit, $n2.unit, 1);
        my Num $n = $n1 * $n2;
        $n does NumUnit;
        $n.unit = %def;
        return $n;
    }

    multi *infix:</>(NumUnit $n1, NumUnit $n2) {
        my Num %def = $.multdef($n1.unit, $n2.unit, -1);
        my Num $n = $n1 / $n2;
        $n does NumUnit;
        $n.unit = %def;
        return $n;
    }

    multi *infix:<**>(NumUnit $n1, NumUnit $n2) {
        if !$n2.is_unitless {
            warn "Non-unitless exponent { $n2.unit }\n";
            return undef;
        }
        #XXX I think NumUnits should numify to include their .unit<factor>
        #    That would allow things like ^3.:as<-2> # (0, -2. -4)
        #    That can already be done, but nonlinear units could
        #    allow e.g. ^3.:as<log10scale> # (1, 10, 100)
        #    How is Numification specified?
        my Num $n = ($n1 * $n1.unit<factor>) ** ($n2 * $n2.unit<factor>);
        $n does NumUnit;
        $n.unit = $n1.unit;
        for $n2.unit.kv -> my Num $u, my Num $p {
            next if $u eq 'factor';
            $n.unit{$u} += $p * $n2 * $n2.unit<factor>;
        }
        $n.unit.v »*=» $n2 * $n2.unit<factor>;
        $n.unit<factor> = 1;
        return $n;
    }
    ...
}

role StrUnit does GenericUnit {

    BEGIN {    #XXX is this wrong?

    # StrUnit has a completely different set of units:
    # Character encoding, Unicode normalization etc.
    #XXX I'm using utf8 as the fundamental encoding unit for now.
    #    Str encoding conversions shouldn't be handled like other
    #    unit conversions (converting to fundamental and back).
    #    Instead there should be a table of converters to go directly
    #    between each pair of encodings or normalization forms.
    $.add_fund_unit: 'utf8';
    $.add_unit: 'utf16', ...;
    $.add_unit: 'utf32', ...;

    #TODO parse unicode tables for normalization stuff
    ...

    }
    ...
}

role StrDispUnit does GenericUnit {

    BEGIN {    #XXX is this wrong?

    # StrDispUnit is for StrPos and StrLen objects.
    # To a StrDispUnit, code, graph, etc. can be converted down to bit 
    # (via the nonlinear unit mechanism) as long as they're associated
    # with a Str that has a valid StrUnit.  This allows you to do math
    # on StrPos and StrLen objects in arbitrary Unicode units.
    $.add_fund_unit: 'bit';
    $.add_unit: 'byte', { :bit :factor(8) };
    $.add_unit: 'code', ...;
    $.add_unit: 'codepoint', 'code';
    $.add_unit: 'graph', ...;
    $.add_unit: 'grapheme', 'graph';
    ...
    }
    ...
}
