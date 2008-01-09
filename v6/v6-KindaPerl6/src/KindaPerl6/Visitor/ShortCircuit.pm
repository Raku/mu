
use v6-alpha;

class KindaPerl6::Visitor::ShortCircuit {
    sub thunk($value,$pad) {
        Sub.new(
                block => Lit::Code.new(
                    pad => COMPILER::inner_pad($KindaPerl6::Visitor::ShortCircuit::last_pad),
                    body => [$value],
                    sig => Sig.new( invocant => undef, positional => [ ] )
                )
        );
    };
    method visit ( $node, $node_name ) {
        my $pass_thunks := {'infix:<&&>'=>1,'infix:<||>'=>1,'infix:<//>'=>1};
        if ($node_name eq 'Apply') && $pass_thunks{($node.code).name}
        {
            my $left := (($node.arguments)[0]).emit(self);
            my $right := (($node.arguments)[1]).emit(self);


            return Apply.new(
                code => $node.code,
                arguments => [ thunk($left),thunk($right) ]
            );

        }
        if    ( $node_name eq 'Lit::Code' )
        {
            $KindaPerl6::Visitor::ShortCircuit::last_pad := $node.pad;
        }
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
