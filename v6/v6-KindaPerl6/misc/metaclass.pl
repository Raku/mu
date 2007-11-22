
# TODO - create ClassMOP.pm visitor to transform OO calls into Class::MOP calls

use Class::MOP;

    Class::MOP::Class->create('Bar' => (
        version      => '0.01',
        superclasses => [ 'Foo' ],
        attributes => [
        ],
        methods => {
        },
    ) );

    Bar->meta->add_method('bar' => sub { 789 });

print Bar->bar;


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
