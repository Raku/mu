package Pugs::Runtime::Capture;
# Documentation in the __END__

use 5.006;
use strict;
use warnings;
use Scalar::Util 'refaddr';
my %_data;

use overload (
    '@{}'    => \&array,
    '%{}'    => \&hash,
    '${}'    => \&scalar,
    'bool'   => \&scalar,
    '&{}'    => \&scalar,
    '""'     => \&scalar,
    '0+'     => \&scalar,
    fallback => 1,
);

sub new {
    my $obj = bless \$_[1], $_[0];
    $_data{ refaddr $obj } = $_[1];
    return $obj;
}

sub DESTROY { delete $_data{refaddr $_[0]}            }
sub data    {        $_data{refaddr $_[0]}            }
sub hash    {        $_data{refaddr $_[0]}->{hash}    }
sub array   {        $_data{refaddr $_[0]}->{array}   }
sub scalar  {        $_data{refaddr $_[0]}->{scalar}  }

1;

__END__

=head1 NAME 

Pugs::Runtime::Capture - Capture object

=head1 METHODS

* array

* hash

* str

* data

- return the internal representation

* bool

* from

* to

=head1 OVERLOADS

* $match->()

- return the capture

* $match->[$n]

- return the positional matches

* $match->{$n}

- return the named matches

* $match ? 1 : 0

- return whether there was a match

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

