
use v6-alpha;

role v6::AST::Base  {

    sub *coerce:as (Match $match, To ::?CLASS) { 
        To.new( :$match );
    }

}
