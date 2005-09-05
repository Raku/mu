# $Header: /home/fergal/my/cvs/Code-Perl/lib/Code/Perl/Expr/Perl.pm,v 1.1 2003/06/17 14:14:21 fergal Exp $

use strict;

package Code::Perl::Expr::Perl;

use base 'Code::Perl::Expr::Base';

use Class::MethodMaker (
    get_set => [qw( -java Perl )]
);

sub eval
{
    my $self = shift;

    my $perl = $self->getPerl;

    if (wantarray)
    {
        my @value = eval $perl;

        die $@ if $@;
    
        return @value;
    }
    else
    {
        my $value = eval $perl;

        die $@ if $@;
    
        return $value;
    }
}

sub perl
{
    my $self = shift;

    return $self->getPerl;
}

1;
