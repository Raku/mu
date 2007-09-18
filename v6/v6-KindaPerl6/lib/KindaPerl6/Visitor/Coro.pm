
use v6-alpha;

=begin

This visitor desugars coroutines into plain code objects.

=end

class KindaPerl6::Visitor::Coro {

    method visit ( $node, $node_name ) {
    
        # XXX TODO
    
        if    ( $node_name eq 'Lit::Code' )
        {
            # does this code contain "yield" or "take" ?
            # or, is it declared with "coro" or "gather" ?
            # XXX coro/gather probably don't mix
            
            # insert state header
            
            # process "return" statements
            
            # replace for-loops
            
            # create Coro object?
            # does the caller need to know this is a coroutine?
            
        };
        return;
    };

}
