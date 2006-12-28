use v6-alpha;

class MiniPerl6::AST {

    sub comp_unit ( $match ) {
        return ::CompUnit(
            'name'        => ($$match){'full_ident'},
            'attributes'  => { },
            'methods'     => { },
            'body'        => ($$match){'exp_stmts'},
        );
    };

}

=begin

=head1 NAME 

MiniPerl6::AST::CompUnit - AST for MiniPerl6 classes

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2006 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end
