
use v6-alpha;

class KindaPerl6::Visitor::EmitAstHTML {

    # This visitor builds a html version of the ".perl" representation of the AST

    method visit ( $node, $node_name ) {
        my $result := '';
        #my $item;
        #my $subitem;
        
        $result := $result ~ '<span class="' ~ Main::mangle_ident( $node_name ) ~ '">';
        
        $result := $result ~ "::" ~ $node_name ~ "( ";
        my $data := $node.attribs;
        for keys %($data) -> $item {
            $result := $result ~ " " ~ $item ~ " => ";
            if ($data{$item}).isa('Array') {
                $result := $result ~ "[ ";
                for @($data{$item}) -> $subitem {
                    if $subitem.isa('Array') {
                        $result := $result ~ ' [ ... ], ';
                    }
                    else {
                        $result := $result ~ $subitem.emit( self ) ~ ", ";
                    };
                };
                $result := $result ~ " ], ";
            } 
            else {
            if ($data{$item}).isa('Hash') {
                $result := $result ~ "{ ";
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
                $result := $result ~ "\'" ~ $data{$item} ~ "\', ";
            } 
            else {
                $result := $result ~ ($data{$item}).emit( self ) ~ ", "; 
            }; 
            };
            };
        };
        $result := $result ~ ") ";
        
        $result := $result ~ '</span>';

    };

}
