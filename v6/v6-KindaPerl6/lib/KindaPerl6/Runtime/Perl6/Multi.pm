use v6-alpha;
class Multi is Code {
    has @.long_names;
    method APPLY {
        say "# oops - multis not implemented yet";
        say "# ", (self.long_names).elems, " long_names";
        
        # @($.long_names)  - subroutine list
        #    .signature
        #        .invocant
        #        .array
        #        .hash
        
        my @candidates;
        my $sub; # XXX 
        for @.long_names -> $sub {
            if ($sub.signature).arity == (@_[0]).arity {
                @candidates.push( $sub );
            };
        };
        
        say '# ', @candidates.elems, ' subs matched the arity';
        
    }
}

