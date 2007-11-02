package Pugs::Compiler::Token;

our $VERSION = '0.28';

use strict;
use warnings;

use base 'Pugs::Compiler::Regex';

sub compile {
    my ( $class, $rule_source, $param ) = @_;
    $param = ref $param ? { %$param } : {}; 
    $param->{ratchet} = 1 
        unless defined $param->{ratchet};
    $class->SUPER::compile( $rule_source, $param );   
}

1;

__END__

=head1 NAME 

Pugs::Compiler::Token - Compiler for Perl 6 Token

=head1 DESCRIPTION

This module provides an implementation for Perl 6 Token.
See L<Pugs::Compiler::Rule> for documentation.

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 Rules Spec: L<http://dev.perl.org/perl6/doc/design/syn/S05.html>

=head1 COPYRIGHT

Copyright 2006 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
