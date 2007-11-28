use v6-alpha;
class Range is List {
    has $.start;
    has $.end;
    method perl {
        '( ' ~ $.start.perl ~ '..' ~ $.end.perl ~ ' )'
    };
    method Str {
        $.start ~ ".." ~ $.end
    };
    method min { $.start };
    method max { $.end };
    #method true { true };
    #method Int  { $.start.Int };
    #method hash {
    #    { $.start => $.end, }
    #};
    #method array {
    #    [ $.start, $.end ]
    #};
    method map ( &code ) {
        my @res;
        my $arity = (&code.signature).arity;
        my $v = $.start;
        while $v <= $.end {
            my @param;
            while @param.elems < $arity {
                @param.push( ( $v <= $.end ) ?? $v !! undef );
                $v = $v + 1;
            }
            @res.push( code( |@param ) );
        };
        @res;
    };
    method INDEX ( $i ) {
        my $v = $i + $.start - 1;
        ( $v ~~ self ) ?? $v !! undef;
    };
    method smartmatch ( $v ) {
        # XXX && bug -- ( $v >= $.start ) && ( $v <= $.end )
        if $v < $.start { return False }
        if $v > $.end   { return False }
        return True;
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
