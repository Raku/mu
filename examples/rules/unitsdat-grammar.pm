module Units;

class Unitdef {
    has Num %.fund_units;
    has Bool $.is_linear;
    # For nonlinear units only
    has Num %.input_units
    #XXX should this just be (Num --> Num), and typechecking done
    #    in the grammar with defeqv instead of here with checkunit?
    has method &.cv_to_fund(NumUnit --> Num);
    has method &.cv_from_fund(NumUnit --> Num);

    multi submethod BUILD(Num :%def) {
        %.fund_units = defreduce(%def);
        $.is_linear = True;
    }

    multi submethod BUILD(Num :$num) {
        %.fund_units = { :factor($num) };
        $.is_linear = True;
    }

    multi submethod BUILD(Num :%def, Num :%input,
        Code :&to_fund:(Num --> Num), Code :&from_fund:(Num --> Num)) {

        $.is_linear = False;
        %.fund_units = defreduce(%def);
        %.input_units = defreduce(%input);
        &.cv_to_fund :=   method (NumUnit $x --> Num) {
                My Num $f = checkunit($x.unit);
                return to_fund($x) * $f;
            };
        &.cv_from_fund := method (NumUnit $x --> Num) {
                My Num $f = checkunit($x.unit);
                return from_fund($x) / $f;
            };
    }

    method checkunit(Num %def1 is copy) {
        my Num %def2 = %.input_units;
        my Num $f1 = %def1.:delete<factor>;
        my Num $f2 = %def2.:delete<factor>;
        die "Nonlinear conversion from wrong unit: { %def1 } should be: { %def2 }\n"
            if %def1 !eqv %def2;
        return $f1 / $f2;
    }
}

grammar UnitsGeneric {

    # Expects to have these attributes available:
    #
    # A list of all valid unit names
    # Str @.units;
    #
    # A list of all unit prefixes (defined in %.unitsdef)
    # Str @.prefixes
    #
    # A list of all nonlinear units
    # Str @.nl_units
    #
    # Definitions of all derived units (and prefixes)
    # in terms of fundamental units (which do not appear)
    # Hash of Unitdef %.unitsdef

    # Perl and units.dat use different syntax for multiplication and power
    # operators.  Those derived grammars need to supply these tokens.
    token m  { { ... } }
    token p  { { ... } }

    # Units.dat has a special fraction syntax; Perl doesn't.
    token fraction { { ... } }

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
            { $<factor> *= %.unitsdef{@<prefix>[$n++]}.fund_units<factor> }
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

    rule TOP {
        [ ^^
            [ <?fundamental_unit> | <?unit> | <?nl_unit> | <?prefix> ]?
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
            %.unitsdef{$<name>} = Unitdef.new(num => $<number><num>);
        }
    }

    token unit {
        $<name> := [ <+[\S]-[-]>+ ] \h+ <unitdef>
        {
            @.units.push: $<name>;
            %.unitsdef{$<name>} = Unitdef.new(def => $<unitdef><def>);
        }
    }

    rule nl_unit {
        $<name> := [ <+[\S]-[(]>+ ]
        '(' $<var> := [ <+[\S]-[)]>+ ] ')'
        [ '[' $<indef> := <unitdef>? ';' $<outdef> := <unitdef>? ']' ]?
        {
            $<in_def>  := $<indef><def> // { :factor };
            $<out_def> := $<outdef><def> // { :factor };
        }
        $<todef> := <nl_varunitdef($<var>, $<in_def>)>
        ';'
        $<fromdef> := <nl_varunitdef($<name>, $<out_def>)>
        {
            die "Nonlinear input unit: { $<todef><def> } should be: { $<in_def> }\n"
                if !$.defeqv($<todef><def>, $<in_def>);
            die "Nonlinear output unit: { $<fromdef><def> } should be: { $<out_def> }\n"
                if !$.defeqv($<fromdef><def>, $<out_def>);
            @.units.push: $<name>;
            @.nl_units.push: $<name>;
            %.unitsdef{$<name>} = Unitdef.new(def => $<out_def>, input => $<in_def>,
                to_fund => $<todef><closure>, from_fund => $<fromdef><closure>);
        }

    }

    rule nl_paren(Str $var, Num %def) {
        '(' <nl_varunitdef($var, %def)> ')' {
            $<closure> := $<nl_varunitdef><closure>;
            $<def> := $<nl_varunitdef><def>;
        }
    }

    rule nl_pow(Str $var, Num %def) {
        [ $<lhs> := <nl_atom($var, %def)>
        | $<lhs> := <nl_paren($var, %def)>
        ]
        <?p>
        [ $<rhs> := <nl_atom($var, %def)>
        | $<rhs> := <nl_paren($var, %def)>
        | $<rhs> := <nl_pow($var, %def)>
        ] {
            die "Non-unitless exponent { $<rhs><def> }\n"
                unless all($<rhs><def>.k) eq any('factor', @.fund_unitless);
            $<def> = $<lhs><def>;
            for $<rhs><def>.kv -> my Str $u, my Num $p {
                next if $u eq 'factor';
                $<def>.{$u} += $p;
            }
            $<def>.v »*=» $<rhs><def><factor>;
            $<closure> := sub (Num $x --> Num) {
                $<lhs><closure>.($x) ** $<rhs><closure>.($x);
            }
        }
    }

    rule nl_mult(Str $var, Num %def) {
        [ $<lhs> := <nl_atom($var, %def)>
        | $<lhs> := <nl_paren($var, %def)>
        | $<lhs> := <nl_pow($var, %def)>
        ]
        [ <?m>  [ $<rhs> := <nl_atom($var, %def)>
                | $<rhs> := <nl_paren($var, %def)>
                | $<rhs> := <nl_pow($var, %def)>
                | $<rhs> := <nl_mult($var, %def)>
                ] {
                    $<def> := $.multdef($<lhs><def>, $<rhs><def>, 1);
                    $<closure> := sub (Num $x --> Num) {
                        $<lhs><closure>.($x) * $<rhs><closure>.($x);
                    }
                }
        | '/'   [ $<rhs> := <nl_atom($var, %def)>
                | $<rhs> := <nl_paren($var, %def)>
                | $<rhs> := <nl_pow($var, %def)>
                | $<rhs> := <nl_mult($var, %def)>
                ] {
                    $<def> := $.multdef($<lhs><def>, $<rhs><def>, -1);
                    $<closure> := sub (Num $x --> Num) {
                        $<lhs><closure>.($x) / $<rhs><closure>.($x);
                    }
                }
        ]
    }

    rule nl_add(Str $var, Num %def) {
        [ $<lhs> := <nl_atom($var, %def)>
        | $<lhs> := <nl_paren($var, %def)>
        | $<lhs> := <nl_pow($var, %def)>
        | $<lhs> := <nl_mult($var, %def)>
        ]
        [ '+'   [ $<rhs> := <nl_atom($var, %def)>
                | $<rhs> := <nl_paren($var, %def)>
                | $<rhs> := <nl_pow($var, %def)>
                | $<rhs> := <nl_mult($var, %def)>
                | $<rhs> := <nl_add($var, %def)>
                ] {
                    $<def> := $.adddef($<lhs><def>, $<rhs><def>, 1);
                    $<closure> := sub (Num $x --> Num) {
                        $<lhs><closure>.($x) + $<rhs><closure>.($x);
                    }
                }
        | '-'   [ $<rhs> := <nl_atom($var, %def)>
                | $<rhs> := <nl_paren($var, %def)>
                | $<rhs> := <nl_pow($var, %def)>
                | $<rhs> := <nl_mult($var, %def)>
                | $<rhs> := <nl_add($var, %def)>
                ] {
                    $<def> := $.adddef($<lhs><def>, $<rhs><def>, -1);
                    $<closure> := sub (Num $x --> Num) {
                        $<lhs><closure>.($x) - $<rhs><closure>.($x);
                    }
                }
        ]
    }

    rule nl_varunitdef(Str $var, Num %def) {
        [ $<nl> := <nl_atom($var, %def)>
        | $<nl> := <nl_paren($var, %def)>
        | $<nl> := <nl_pow($var, %def)>
        | $<nl> := <nl_mult($var, %def)>
        | $<nl> := <nl_add($var, %def)>
        ] {
            $<closure> := $<nl><closure>;
            $<def> := $<nl><def>;
        }
    }

    rule nl_func(Str $var, Num %def) {
        | $<name> := [ | @.nl_units ]
            $<inner> := <nl_paren($var, %def)>
            {
                My Unitdef $u := %.unitsdef{$<name>};
                die "Nonlinear input unit: { $<inner><def> } should be: { $u.input_units }\n"
                    if !$.defeqv($<inner><def>, $u.input_units);
                $<closure> := sub (Num $x --> Num) {
                    $u.cv_to_fund($<inner><closure>.($x));
                }
                $<def> := $u.fund_units;
            }
        # ~nlfunc(...) means the inverse conversion function
        | '~' $<name> := [ | @.nl_units ]
            $<inner> := <nl_paren($var, %def)>
            {
                My Unitdef $u := %.unitsdef{$<name>};
                die "Nonlinear input unit: { $<inner><def> } should be: { $u.fund_units }\n"
                    if !$.defeqv($<inner><def>, $u.fund_units);
                $<closure> := sub (Num $x --> Num) {
                    $u.cv_from_fund($<inner><closure>.($x));
                }
                $<def> := $u.input_units;
            }
    }

    token nl_atom(Str $var, Num %def) {
        [ $<nl> := <nl_unitdef>
        | $<nl> := <var($var, %def)>
        | $<nl> := <nl_func($var, %def)>
        ] {
            $<closure> := $<nl><closure>;
            $<def> := $<nl><def>;
        }
    }

    token nl_unitdef {
        <unitdef> {
            $<closure> := sub (Num $x --> Num) { 1 };
            $<def> := $<unitdef><def>;
        }
    }

    token var(Str $var,  Num %def) {
        <$var> {
            $<closure> := sub (Num $x --> Num) { $x };
            $<def> = %def;
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
    has Str @.nl_units;
    has Str @.prefixes;
    has Hash of Unitdef %.unitsdef;
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
    has Num %.unit is rw = { :factor };

    # is this object unitless, e.g. can it be used as the arg to cos or exp?
    method is_unitless(--> Bool) {
        return !defined %.unit || all(%.unit.k) eq any('factor', @.fund_unitless);
    }

    method defeqv(Num %def1 is copy, Num %def2 is copy --> Bool) {
        %def1.:delete<factor>;
        %def2.:delete<factor>;
        return %def1 eqv %def2;
    }

    method add_prefix(Str $name, Num $value -->) {
        @.prefixes.push: $name;
        %.unitsdef{$name} = Unitdef.new(num => $value);
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
        %.unitsdef{$name} = Unitdef.new(def => %def);
    }

    multi method add_unit(Str $name, Str $unitspec -->) {
        if ! $unitspec ~~ /<UnitsPerl>/ {
            die "Couldn't parse $unitspec\n";
        }
        @.units.push: $name;
        %.unitsdef{$name} = Unitdef.new(def => $<def>);
    }

    method add_nl_unit(Str $name, Num %def, Num %input,
        Code :&to_fund:(Num --> Num), Code :&from_fund:(Num --> Num)) {

        @.units.push: $name;
        @.nl_units.push: $name;
        %.unitsdef{$name} = Unitdef.new(:%def, :%input, :&to_fund, :&from_fund);
    }

    # reduce a unit definition to fundamental units
    method defreduce(Num %def is copy --> Hash of Num) {
        until all(%def.k) eq any('factor', @.fund_units, @.fund_unitless) {
            for %def.kv -> (my Str $u, my Num $p) {
                next if $u eq any('factor', @.fund_units, @.fund_unitless);
                %def.:delete{$u};
                %def<factor> *= %.unitsdef{$u}.fund_units<factor> ** $p;
                for %.unitsdef{$u}.fund_units.kv -> (my Str $cu, my Num $cp) {
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
        my Num $f1 = %def1c.:delete<factor>;
        my Num $f2 = %def2c.:delete<factor>;
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
    #XXX handle nonlinear units
    multi method :as(Num $self: Str $unitspec --> NumUnit) {
        $self does NumUnit;
        if ! $unitspec ~~ /<UnitsPerl>/ {
            die "Couldn't parse $unitspec\n";
        }
        %.unit = $<def>;
        return self;
    }

    # Convert:
    # ($six_feet - $two_mm).:as<furlongs>
    # (4.:as<bits> * 2.5.:as<GHz>).:as<ns/(1500*bytes)>
    #XXX handle nonlinear units
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

    #XXX I think NumUnits should numify to include their .unit<factor>
    #    That would allow things like ^3.:as<-2> # (0, -2. -4)
    #    That can already be done, but nonlinear units could
    #    allow e.g. ^3.:as<log10scale> # (1, 10, 100)
    #    How is Numification specified?
    #XXX handle nonlinear units
    multi *prefix<+>(NumUnit $n --> Num) {
        return $n * $n.unit<factor>;
    }

    multi *infix:<+>(NumUnit $n1, NumUnit $n2) {
        my Num %def = $.adddef($n1.unit, $n2.unit, 1);
        my Num $n = +$n1 + +$n2;
        $n does NumUnit;
        $n.unit = %def;
        return $n;
    }

    multi *infix:<->(NumUnit $n1, NumUnit $n2) {
        my Num %def = $.adddef($n1.unit, $n2.unit, -1);
        my Num $n = +$n1 - +$n2;
        $n does NumUnit;
        $n.unit = %def;
        return $n;
    }

    multi *infix:<*>(NumUnit $n1, NumUnit $n2) {
        my Num %def = $.multdef($n1.unit, $n2.unit, 1);
        my Num $n = +$n1 * +$n2;
        $n does NumUnit;
        $n.unit = %def;
        return $n;
    }

    multi *infix:</>(NumUnit $n1, NumUnit $n2) {
        my Num %def = $.multdef($n1.unit, $n2.unit, -1);
        my Num $n = +$n1 / +$n2;
        $n does NumUnit;
        $n.unit = %def;
        return $n;
    }

    multi *infix:<**>(NumUnit $n1, NumUnit $n2) {
        if !$n2.is_unitless {
            warn "Non-unitless exponent { $n2.unit }\n";
            return undef;
        }
        my Num $n = +$n1 ** +$n2;
        $n does NumUnit;
        $n.unit = $n1.unit;
        for $n2.unit.kv -> my Str $u, my Num $p {
            next if $u eq 'factor';
            $n.unit{$u} += $p;
        }
        $n.unit.v »*=» +$n2;
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
