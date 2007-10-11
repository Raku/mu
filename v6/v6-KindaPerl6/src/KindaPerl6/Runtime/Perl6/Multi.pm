
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
    has @.long_names;    # sub/method multi-dispatch
    has %.token_length;  # token:sym  multi-dispatch
            
    method add_variant ( $code ) {

        #say "# add_variant: ", $code.WHAT;

        if !( defined( self.long_names ) ) {
            self.long_names = [ ];   # XXX accessor init bug
        };

        (self.long_names).push( $code );
    }
            
    method add_token_variant ( $code, $sym ) {
    
        # XXX - fixme
        # http://moritz.faui2k3.org/irclog/out.pl?channel=perl6;date=2007-09-17#i_104660
        # <TimToady> sym is a way of specifying the identifier of some tokens and later some operator names, 
        #   but it's not the definition of longest token
        #   if the rule says "<sym> 'bar'" then the token would be $bar (at least)
        #   it's basically everything DFAable up to <ws> or ::

        my $len = $sym.chars;
        #say "add_token_variant: $sym : $len ";
        if !( defined( self.token_length ) ) {
            self.token_length = { };   # XXX accessor init bug
        };
        if !( defined( ( self.token_length ){$len} ) ) {
            ( self.token_length ){$len} = { };
        };
        ( ( self.token_length ){$len} ){ $sym } = Multi.new();
        ( ( ( self.token_length ){$len} ){$sym} ).add_variant( $code );
    }
            
    method select ( $capture ) {
    
        #say "# capture isa ", $capture.WHAT;
        #say "capture: $capture";

        unless $capture.isa( 'Capture' ) { 
            die "the parameter to Multi.select must be a Capture";
        };

        # token:sym dispatch

        if     defined( self.token_length ) 
            && ( self.token_length ).keys 
        {
            my @len = ( ( self.token_length ).keys );
            #say "lengths : @len[] ";
            @len = @len.sort( sub { @_[1] <=> @_[0] } );
            #say "lengths : @len[] ";
            #say "string: $_";
            #say "string len: ",$_.chars;

            for @len -> $len {
                if ( $_.chars ) >= $len {
                    my $s = substr( $_, 0, $len );   # XXX - pos?
                    #say "# len: $len - $s";
                    my %syms = ( self.token_length ){$len};
                    #say "# syms: ", ( %syms.keys );

                    for %syms.keys -> $sym {
                        if $s eq $sym {
                            #say "found!";
                            return ( %syms{$sym} ).select( $capture );
                        }
                    }
                };
            };
            #say "done";
        };

        # sub/method dispatch
        
        if !( defined( self.long_names ) ) {
            die "can't resolve Multi dispatch";
        };

        my @candidates = [ ];
        #say '# testing ', (self.long_names).elems, " candidates in ", (self.long_names).WHAT;
        
        for @(self.long_names) -> $sub {
            #say "# testing sub "; #, $sub;
            #say $sub.WHAT;
            #say "signature: ", $sub.signature, "\n";
            if ($sub.signature).arity == $capture.arity {
                @candidates.push( $sub );
            };
        };
        
        #say '# ', @candidates.elems, ' subs matched the arity';
        if @candidates.elems == 1 {
            return @candidates[0];
        };
        
        #say "capture: ", $capture;
        die "can't resolve Multi dispatch";
    }
    
    method perl {
        'Multi.new( ... )'
    }
}

