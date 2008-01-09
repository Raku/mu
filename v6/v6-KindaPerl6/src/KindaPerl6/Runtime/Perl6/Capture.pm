use v6-alpha;
class Capture is Value {
    has $.invocant;
    has $.array;
    has $.hash;

    method arity {
        # ??? how about optionals
        @.array.elems + $.hash.elems;
    };
    method perl {
        my $s = '\\( ';

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
    method LOOKUP($key) {
        if (!(defined($.hash))) {
            $.hash = {};
        };
        return $.hash{$key};
    };
    #method true { self.elems != 0 };
    #method Int  { self.elems };
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
