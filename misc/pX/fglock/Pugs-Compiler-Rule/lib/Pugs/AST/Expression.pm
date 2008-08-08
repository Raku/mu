package Pugs::AST::Expression;

use strict;
use warnings;
use Data::Dumper;

sub term {
    { term => $_[1]{'term'}->() ,}
}

sub operator {
    my $self = shift;
    my $match = shift;
    # die "not a match" unless ref($match) eq 'Pugs::Runtime::Match';
    my %h = %$match;
    my %opt = @_;
    
    #print "capture ", Dumper($match) if $opt{'fixity'} eq 'circumfix';
    for ( keys %h ) {
        $h{$_} = $h{$_}->();
    }
    my @a = @$match;
    if ( @a ) {
        $a = shift @a;
        for ( @$a ) {
            $_ = $self->operator( $_ );
            ${$_}{fixity} = $opt{fixity};
        }
        $h{list} = $a;
        delete $opt{fixity};
    }
    $h{$_} = $opt{$_} for keys %opt;
    return \%h;
}

1;
