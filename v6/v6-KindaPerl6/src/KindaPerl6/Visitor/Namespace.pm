
use v6-alpha;

=begin

This visitor maps global variables into a namespace-hash.

=end

class KindaPerl6::Visitor::Namespace {

    method visit ( $node, $node_name ) {

        if    ( $node_name eq 'Var' )
        {
            if @($node.namespace) {
                #say "global ", $node.name;
                # $X::Y::z -> %KP6<X::Y><Scalar_z>
                return ::Call(
                     'invocant' => ::Call(
                             'invocant' => ::Var(
                                    namespace => [ 'GLOBAL' ],
                                    name      => 'KP6',
                                    twigil    => '',
                                    sigil     => '%',
                                ),
                             'arguments' => [ ::Val::Buf( buf => ($node.namespace).join('::') ) ],
                             'method' => 'LOOKUP',
                             'hyper' => ''
                        ),
                     'arguments' => [ ::Val::Buf( buf => ( $node.sigil ~ $node.name) ) ],
                     'method' => 'LOOKUP',
                     'hyper' => ''
                );
            }
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
