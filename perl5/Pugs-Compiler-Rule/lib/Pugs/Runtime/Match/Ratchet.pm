package Pugs::Runtime::Match::Ratchet;
# Documentation in the __END__

use 5.006;
use strict;
use warnings;
use Data::Dumper;

use overload (
    '@{}'    => \&array,
    '%{}'    => \&hash,
    'bool'   => \&bool,
    '&{}'    => \&code,
    '""'     => \&flat,
    '0+'     => \&flat,
    fallback => 1,
);

# unused!
sub new {
    my ($class, $match) = @_;
    my $self = \$match;
    bless $self, $class;
}

sub from  {  ${${$_[0]}->{from}} }
sub to    {  ${${$_[0]}->{to}}   }
sub bool  {  ${${$_[0]}->{bool}} }
sub hash  {  ${$_[0]}->{named}   }
sub array {  ${$_[0]}->{match}   }

sub flat {
    return ${ ${$_[0]}->{capture} }
        if defined ${ ${$_[0]}->{capture} };
    return substr( ${${$_[0]}->{str}}, $_[0]->from, $_[0]->to - $_[0]->from );
}

# return the capture
sub code {
    my $c = $_[0]->flat;
    return sub { $c };
}

1;

__END__

=head1 NAME 

Pugs::Runtime::Match::Ratchet - Match object created by :ratchet rules

=head1 SEE ALSO

Pugs::Runtime::Match

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 COPYRIGHT

Copyright 2006 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
