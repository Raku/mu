
use v6-alpha;

class KindaPerl6::Visitor::Perl {

    # This visitor builds a ".perl" representation of the AST

    method visit ( $node, $node_name, $data ) {
        my $result := '';
        #my $item;
        #my $subitem;
        $result := $result ~ "::" ~ $node_name ~ "( ";
        for keys %($data) -> $item {
            $result := $result ~ " " ~ $item ~ " => ";
            if ($data{$item}).isa('Array') {
                $result := $result ~ "[ ";
                for @($data{$item}) -> $subitem {
                    $result := $result ~ $subitem.emit( self ) ~ ", ";
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
                $result := $result ~ ($data{$item}).emit( self ); 
            }; 
            };
            };
        };
        $result := $result ~ ") ";
    };

}
