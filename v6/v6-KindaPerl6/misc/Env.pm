
package Env;
use strict;

sub new {
    bless {
        pads => [
            sub { eval $_[0] },
        ],
        variable_names => [
            [ ],
        ],
    }, $_[0];
}

sub get_levels {
    my $self = shift;
    scalar @{$self->{pads}};
}

sub eval {
    my $self = shift;
    $self->{pads}[-1]( $_[0] );
}

sub get_variables {
    my $self = shift;
    $self->eval(
        join ',', map {
            ( "'$_'", $_ )
        } @{$self->{variable_names}[-1]}
    );
}

sub add_pad {
    my $self = shift;
    push @{$self->{variable_names}}, [@_];
    push @{$self->{pads}}, $self->eval(
           (scalar @_ ? 'my (' . (join ',', @_ ) . '); ' : '')
        .  'sub { ' . (join ',', @_ ) . '; eval $_[0] } '
    );
}

sub drop_pad {
    my $self = shift;
    pop @{$self->{pads}};
    pop @{$self->{variable_names}};
}

1;


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
