use v6-alpha;

=head1 NAME

Automata::Cellular::rule - Build and render Cellular Automata Wolfram-style
rules

=head1 VERSION

Version 0.1

=head1 DESCRIPTION

C<Automata::Cellular> is the meat of this module; everything you need for
most investigations should be included by:

    use Automata::Cellular;

More information can also be found in the Perldoc for that module.

=head1 AUTHOR

David Brunton - dbrunton@plusthree.com

=cut

role Automata::Cellular::Rule {

    has Bool %.rule;
    has Int $.rule_number;

    # submethod unpacks the rule number into a hash
    submethod BUILD (Int $.rule_number) {
        for ( 0 .. 7 ) -> $key {
            %.rule{$key} = ?($.rule_number +& (1 ~ 0 x $key) );
        }
    }

    # for "pretty" (being a relative term) output of the rule
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
            
