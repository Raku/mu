use v6-alpha;

class KindaPerl6::Visitor::Optimize::RuleConstantFold {

    method visit ($node, $node_name) {
        if $node.isa('Rule::Concat')
        {
            # The nodes of the concat rule. Put here because we're
            # rewriting it.
            my $nodes;

            # Save constants if we're looping through them.
            my $constant := '';

            for @($node.concat) -> $stmt {

                if $stmt.isa('Rule::Constant')
                {
                    $constant := $constant ~ $stmt.constant;
                }
                else
                {
                    # If we've been looping through constants then
                    # dump them
                    if ($constant ne '')
                    {
                        push @$nodes, ::Rule::Constant(
                            constant => $constant,
                        );
                        $constant := '';
                    };
                    push @$nodes, $stmt.emit(self);
                }
            }

            if (($constant ne '') && !$nodes)
            {
                return ::Rule::Concat(
                    concat => ::Rule::Constant(
                        constant => $constant,
                    ),
                );
            }
            else
            {
                return ::Rule::Concat(
                    concat => $nodes,
                );
            }
        }
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
