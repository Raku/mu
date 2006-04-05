package Pugs::AST::Expression;

use strict;
use warnings;
use Data::Dumper;

sub term {
    { term => $_[1]{'term'}->() ,}
}

sub operator {
    my $self = $_[0];
    # die "not a match" unless ref($_[1]) eq 'Pugs::Runtime::Match';
    my %h = %{$_[1]};
    for ( keys %h ) {
        $h{$_} = $h{$_}->();
    }
    my @a = @{$_[1]};
    if ( @a ) {
        $a = shift @a;
        for ( @$a ) {
            $_ = $self->operator( $_ );
        }
        $h{list} = $a;
    }
    return \%h;
}

1;
