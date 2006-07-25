
use v6-alpha;

class v6::AST::Base  {

    sub *coerce:as (Match $match, To ::v6::AST::Base) { 
        To.new( :$match );
    }

}
