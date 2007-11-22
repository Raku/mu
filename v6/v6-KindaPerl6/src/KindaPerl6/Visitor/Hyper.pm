
use v6-alpha;

=begin

This visitor transforms hyper method calls into map() calls.

It allows the implementation of '.>>' in the perl5 and parrot backends.

Before:

    @abc.>>x()

After:

    map { .x() }, @abc

=end

class KindaPerl6::Visitor::Hyper {

    method visit ( $node, $node_name ) {
        if    ( $node_name eq 'Call' )
           && ( $node.hyper )
        {
            return ::Apply(
                code      => 'map',
                arguments => [
                    ::Sub(
                        sig   => ::Sig( invocant => undef, positional => [ ] ),
                        block => [
                            ::Call(
                                invocant  => ::Var( sigil => '$', twigil => '', name => '_' ),
                                method    => $node.method,
                                arguments => $node.arguments,
                            ),
                        ]
                    ),
                    $node.invocant,
                ]
            );
        };
        return;
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
