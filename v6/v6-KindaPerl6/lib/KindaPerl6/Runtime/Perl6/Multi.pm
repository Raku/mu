
        # TODO for Multi:
        # - work on non-capture calls, for temporary backwards compatibility
        # - dispatch on types, 
        # - dispatch on named args
        # - integrate with the Grammar
        # - how to make it work with same-name multis on outer Pads
            
        # @($.long_names)  - subroutine list
        #    .signature
        #        .invocant
        #        .array
        #        .hash

use v6-alpha;
class Multi is Code {
    has @.long_names;
            
    method add_variant ( $code ) {
        (self.long_names).push( $code );
    }
            
    method select {
        my @candidates;
        my $sub; # XXX 
        
        unless (@_[0]).isa( 'Capture' ) { 
            die "the parameter to Multi.select must be a Capture";
        };

        for @(self.long_names) -> $sub {
            #say "# testing sub ", $sub;
            if ($sub.signature).arity == (@_[0]).arity {
                @candidates.push( $sub );
            };
        };
        
        #say '# ', @candidates.elems, ' subs matched the arity';
        if @candidates.elems == 1 {
            return @candidates[0];
        };
        
        die "can't resolve Multi dispatch";
    }
    
    method perl {
        'Multi.new( ... )'
    }
}

