
use v6-alpha;

grammar KindaPerl6::Grammar {

    token stmt_sep {
       <.opt_ws> \; <.opt_ws> | \n <.opt_ws>
    };
    token exp_stmts {
        | <exp>
            [
            |   <.stmt_sep> <exp_stmts>
                [<.stmt_sep> | <.opt_ws>]
                { make [ $$<exp>, @( $$<exp_stmts> ) ] }
            |   [<.stmt_sep> | <.opt_ws>]
                { make [ $$<exp> ] }
            ]
        | { make [] }
    };
    token exp_stmts2 {
        <exp>
            [
            |   <.stmt_sep> <exp_stmts>
                [<.stmt_sep> | <.opt_ws>]
                { make [ $$<exp>, @( $$<exp_stmts> ) ] }
            |   [<.stmt_sep> | <.opt_ws>]
                { make [ $$<exp> ] }
            ]
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
