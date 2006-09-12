use v6-alpha;

role Automata::Cellular::Rule {

    has Bool %.rule;
    has Int $.rule_number;

    submethod BUILD (Int $.rule_number) {
        for ( 0 .. 7 ) -> $key {
            %.rule{$key} = ?($.rule_number +& (1 ~ 0 x $key) );
        }
    }

    method pretty (Str $true = 'x', Str $false = '.') {
        my Str $rule_string = '';
        for %.rule.kv -> $k,$v { 
             $rule_string ~= "{sprintf("%03b",$k)} becomes {+$v}\n";
        }
        $rule_string ~~ s:g/1/$true/;
        $rule_string ~~ s:g/0/$false/;
        return $rule_string;
    }

}
            
