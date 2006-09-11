use v6-alpha;

role Automata::Cellular::Rule {

    has Bool %.rule;
    has Int $.rule_number;

    submethod BUILD (Int $.rule_number) {
        for ( 0 .. 7 ) -> $key {
            %.rule{$key} = ?($.rule_number +& (1 ~ 0 x $key) );
        }
    }

    method prettyprint {
        for %.rule.kv -> $k,$v { 
            say sprintf("%03b",$k) ~ " becomes " ~ +$v;
        }
    }

}
            
