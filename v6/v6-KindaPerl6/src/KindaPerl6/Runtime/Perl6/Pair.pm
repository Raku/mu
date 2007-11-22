use v6-alpha;
class Pair is Value {
    has $.key;
    has $.value;
    method perl {
        '( ' ~ $.key.perl ~ ' => ' ~ $.value.perl ~ ' )'
    };
    method Str {
        $.key ~ "\t" ~ $.value
    };
    method true { true };
    #method kv   { ( $.key, $.value ) };
    method Int  { $.value.Int };
    #method hash {
    #    { $.key => $.value, }
    #};
    method array {
        [ $.key, $.value ]
    };
    method keys {
        [ $.key ]
    };
    method values {
        [ $.value ]
    };
    method LOOKUP {
           ( @_[0] eq $.key )   # XXX use === instead
        ?? $.value
        !! undef;
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
