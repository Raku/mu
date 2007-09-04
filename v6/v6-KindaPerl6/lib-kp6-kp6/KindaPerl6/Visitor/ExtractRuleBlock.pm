
use v6-alpha;

class KindaPerl6::Visitor::ExtractRuleBlock {

    has $.count;
    method visit ( $node, $node_name, $path ) {
    
        #say "Global ",$node_name;
    
        if    ( $node_name eq 'Rule::Block' )
        {
            use Data::Dumper;
            my $comp_unit := $path[0-1];
            $.count = $.count + 1;
            my $name := '__rule_block'~$.count~$COMPILER::source_md5;
            push @(($comp_unit.body).body), ::Method(block=>$node.closure,name=>$name);
            $node.closure($name);
            return $node;
        }
    };

}
