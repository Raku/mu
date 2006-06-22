
package Pugs::Runtime::Perl6;

use strict;
use warnings;

# TODO - see Pugs::Runtime::Grammar for metaclass stuff

package Pugs::Runtime::Perl6::Alias::Scalar;

sub TIESCALAR {
    my $class = shift;
    my $var_ref = shift;
    return bless $var_ref, $class;
}
sub FETCH {
    my $self = shift;
    $$self;
}
sub STORE {
    my $self = shift;
    $$self = shift;
  }

1;

__END__

=pod

=head1 NAME 

Pugs::Runtime::Perl6

=head1 DESCRIPTION

Provides runtime routines for the Perl6-in-Perl5 compiled code

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 COPYRIGHT

Copyright 2006 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
