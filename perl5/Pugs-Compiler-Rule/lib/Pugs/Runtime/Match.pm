package Pugs::Runtime::Match;
# Documentation in the __END__

use 5.006;
use strict;
use warnings;
use Data::Dumper;
#use Class::InsideOut qw( public register id );
use Scalar::Util 'refaddr';

use overload (
    '@{}'    => \&array,
    '%{}'    => \&hash,
    'bool'   => \&bool,
    '&{}'    => \&code,
    '${}'    => \&scalar,
    '""'     => \&flat,
    '0+'     => \&flat,
    fallback => 1,
);

# class method
# ::fail can be called from inside closures
# sub ::fail { $::_V6_SUCCEED = 0 }

my %_data;

sub new {
    my $obj = bless \$_[1], $_[0];
    $_data{ refaddr $obj } = $_[1];
    return $obj;
}

sub DESTROY {  
    delete $_data{ refaddr $_[0] };
}

sub data  {    $_data{refaddr $_[0]}           }
sub from  {  ${$_data{refaddr $_[0]}->{from}}  }
sub to    {  ${$_data{refaddr $_[0]}->{to}}    }
sub bool  {  ${$_data{refaddr $_[0]}->{bool}}  }
sub hash  {    $_data{refaddr $_[0]}->{named}  }
sub array {    $_data{refaddr $_[0]}->{match}  }

sub flat {
    my $obj = $_data{refaddr $_[0]};
    my $cap = $obj->{capture};
    #print ref $cap;
    return $$cap
        if ref $cap eq 'REF'   ||
           ref $cap eq 'SCALAR';
    return '' unless ${$obj->{bool}};
    
    return '' if $_[0]->from > length( ${$obj->{str}} );
    
    return substr( ${$obj->{str}}, $_[0]->from, $_[0]->to - $_[0]->from );
}

sub str {
    $_[0]->flat;
}

sub perl {
    local $Data::Dumper::Terse    = 1;
    local $Data::Dumper::Sortkeys = 1;
    local $Data::Dumper::Pad = '  ';
    return __PACKAGE__ . "->new( " . Dumper( $_[0]->data ) . ")\n";
}

# tail() for backwards compatibility
# - doesn't work on failed matches
sub tail {
    return substr( ${$_data{refaddr $_[0]}->{str}}, $_[0]->to );
}

# state() is used for multiple matches and backtracking control
sub state {
    return $_data{refaddr $_[0]}->{state};
}

# return the capture
sub code {
    my $c = $_[0];
    return sub { $c->flat };
}

# return the capture
sub scalar {
    return \( $_[0]->flat );
}

1;

__END__

=head1 NAME 

Pugs::Runtime::Match - Match object created by rules

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

