
use v6-alpha;

=begin

This visitor desugars coroutines into plain code objects.

=end

class KindaPerl6::Visitor::Coro {

    method visit ( $node, $node_name ) {

        # XXX TODO

        if    ( $node_name eq 'Lit::Code' )
        {
            # does this code contain "yield" or "take" ?
            # or, is it declared with "coro" or "gather" ?
            # XXX coro/gather probably don't mix

            # insert state header

            # process "return" statements

            # replace for-loops

            # create Coro object?
            # does the caller need to know this is a coroutine?

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
