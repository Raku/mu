
use v6-alpha;

=begin

This visitor maps lexical variables into a Scope object (see Runtime::Perl6::Scope).

=end

class KindaPerl6::Visitor::Scope {

    method visit ( $node, $node_name ) {

        # TODO !!!

        if    ( $node_name eq 'Lit::Code' )
        {

            return ::Lit::Code(
                pad   => $node.pad,
                state => $node.state,
                sig   => $node.sig,
                body  => [

                    # $MY = $MY.inner
                    ::Assign(
                        parameters => ::Var(
                            namespace => [],
                            name      => 'MY',
                            twigil    => '',
                            sigil     => '$',
                        ),
                        arguments => ::Call(
                            hyper     => undef,
                            arguments => undef,
                            method    => 'inner',
                            invocant  => ::Var(
                                namespace => [],
                                name      => 'MY',
                                twigil    => '',
                                sigil     => '$',
                            ),
                        ),
                    ),

                    @($node.body),

                    # $MY = $MY.outer
                    ::Assign(
                        parameters => ::Var(
                            namespace => [],
                            name      => 'MY',
                            twigil    => '',
                            sigil     => '$',
                        ),
                        arguments => ::Call(
                            hyper     => undef,
                            arguments => undef,
                            method    => 'outer',
                            invocant  => ::Var(
                                namespace => [],
                                name      => 'MY',
                                twigil    => '',
                                sigil     => '$',
                            ),
                        ),
                    ),

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
