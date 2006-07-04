
package Pugs::Runtime::Perl6;

use strict;
use warnings;

use Data::Dumper;
use Data::Bind;

# TODO - see Pugs::Runtime::Grammar for metaclass stuff

sub perl {
    local $Data::Dumper::Terse = 1;
    return join( ', ', Data::Dumper::Dumper( @_ ) );
}

package Pugs::Runtime::Perl6::Routine;
use B ();
use Devel::Caller ();

sub new {
    my ($class, $cv) = @_;
    bless { cv => B::svref_2object($cv) }, $class;
}

sub name {
    my $self = shift;
    my $cv = $self->{cv};
    return '&'.$cv->GV->STASH->NAME . '::' . $cv->GV->NAME;
}

sub package {
    $_[0]->{cv}->GV->STASH->NAME;
}

package Pugs::Runtime::Perl6::Scalar;

sub defined { CORE::defined(@_) }

sub isa {
    my $self = $_[0];
    return 1 if $_[1] eq 'Str'  && defined $_[0];
    return 1 if $_[1] eq 'Num'  && defined $_[0];
    return 1 if $_[1] eq 'Code' && ref($_[0]) eq 'CODE';
    return 0;
}

package Pugs::Runtime::Perl6::Scalar::Alias;

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
