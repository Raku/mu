use v6-alpha;

# See also: S06, AST.pm, Grammar::Signature.pm

class Signature::Item {
    has $.sigil;
    has $.twigil;
    has $.name;
    has $.value;

    has $.is_named_only;
    has $.is_optional;  # !is_required
    # has $.is_longname;
    has $.is_slurpy;
    has $.is_multidimensional;
    has $.is_rw;
    has $.is_copy;

    method perl {
          ( $.is_named_only       ?? ':' !! '' )
        ~ ( $.is_slurpy           ?? '*' !! '' )
        ~ ( $.is_multidimensional ?? '@' !! '' )
        ~ $.sigil ~ $.twigil ~ $.name
        ~ ( $.is_optional         ?? '?' !! '!' )
        ~ ( defined( $.value )    ?? ' = ' ~ $.value.perl !! '' )
        ~ ( $.is_rw               ?? ' is rw' !! '' )
        ~ ( $.is_copy             ?? ' is copy' !! '' )
    }
}

class Signature is Value {
    has $.invocant;
    has @.array;
    #has $.hash;
    has $.return;  # ???

    method arity {
        # ??? how about optionals

        if !( defined( self.array ) ) {
            self.array = [ ];   # XXX accessor init bug
        };

        #if !( defined( self.hash ) ) {
        #    self.hash = { };   # XXX accessor init bug
        #};

        @.array.elems;   # + $.hash.elems;
    };
    method perl {
        my $v;   # XXX kp6 ast processor bug
        my $s = ':( ';

        if $.invocant.defined {
            $s = $s ~ $.invocant.perl ~ ': ';
        };

        for @.array -> $v {
            $s = $s ~ $v.perl ~ ', ';
        };

        # TODO
        #for $.hash.pairs -> $v {
        #    $s = $s ~ $v.perl ~ ', ';
        #};

        return $s ~ ' )'
    };
    method Str {
        self.perl;
    };
}


=begin

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2007 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end
