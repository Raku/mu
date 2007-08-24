use v6-alpha;
class Multi is Code {
    has $.long_names;
    method APPLY {
        say "oops - multis not implemented yet";
        
        # @($.long_names)  - subroutine list
        #    .signature
        #        .invocant
        #        .array
        #        .hash
        
        my $candidates;
        my $sub; # XXX 
        for @( $.long_names ) -> $sub {
            if ($sub.signature).arity == ((@_[0]).signature).arity {
                push @($candidates), $sub;
            };
        };
        
        say $candidates.elems, ' subs matched the arity';
        
    }
}

