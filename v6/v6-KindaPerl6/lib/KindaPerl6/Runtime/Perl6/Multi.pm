
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
    has @.long_names;    # normal sub and method multi-dispatch
    has %.token_length;  # 'sym'-based token multi-dispatch
            
    method add_variant ( $code ) {
        (self.long_names).push( $code );
    }
            
    method add_token_variant ( $code, $sym ) {
        my $len = $sym.chars;
        #say "add_token_variant: $sym : $len ";
        if !( defined( self.token_length ) ) {
            self.token_length = { };   # XXX accessor init bug
        };
        if !( defined( ( self.token_length ){$len} ) ) {
            #say "new length : $len ";
            ( self.token_length ){$len} = Multi.new();
            ( ( self.token_length ){$len} ).long_names = [ ];   # XXX accessor init bug
        };
        my $multi = ( self.token_length ){$len};
        $multi.add_variant( $code );
    }
            
    method select {

        # TODO - dispatch on token_length if it is defined

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

