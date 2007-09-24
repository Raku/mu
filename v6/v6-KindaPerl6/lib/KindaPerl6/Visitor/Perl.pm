
use v6-alpha;

class KindaPerl6::Visitor::Perl {

    # This visitor builds a ".perl" representation of the AST

    method visit ( $node, $node_name ) {

        if $node.isa('Array') {
            my $result := '';
            $result := $result ~ "[ ";
            my $subitem;
            for @($node) -> $subitem {
                if $subitem.isa('Array') {
                    $result := $result ~ self.visit( $subitem, 'Array' ) ~ ', ';
                }
                else {
                if $subitem.isa('Str') {
                    $result := $result ~ "\'" ~ $subitem ~ "\', ";
                }
                else {
                    if ($subitem) {
                        $result := $result ~ $subitem.emit( self ) ~ ", ";
                    }
                };
                };
            };
            return $result ~ " ]";
        };
    
        if $node.isa('Str') {
            return "\'" ~ $node ~ "\'";
        };
        
        my $result := '';
        $result := $result ~ "::" ~ $node_name ~ "( ";
        my $data := $node.attribs;
        my $item;
        for keys %($data) -> $item {
            $result := $result ~ " " ~ $item ~ " => ";
            if ($data{$item}).isa('Array') {
                $result := $result ~ self.visit( $data{$item}, 'Array' ) ~ ", ";
            }
            else { 
            if ($data{$item}).isa('Hash') {
                $result := $result ~ "{ ";
                my $subitem;
                for keys %($data{$item}) -> $subitem {
                    $result := $result 
                        ~ $subitem 
                        ~ ' => '
                        ~ (($data{$item}){$subitem}).emit( self ) 
                        ~ ", ";
                };
                $result := $result ~ " }, ";
            } 
            else {
            if ($data{$item}).isa('Str') {
                $result := $result ~ self.visit( $data{$item}, 'Str' ) ~ ", ";
            } 
            else {
                if ($data{$item}) {
                $result := $result ~ ($data{$item}).emit( self ) ~ ", "; 
                }
            };
            };
            };
        };
        $result := $result ~ ") ";
    };

}
