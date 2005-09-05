# $Header: /home/fergal/my/cvs/Code-Perl/lib/Code/Perl/Expr/DerefHash.pm,v 1.1 2003/06/17 14:14:21 fergal Exp $

use strict;

package Code::Perl::Expr::DerefHash;

use base qw( Code::Perl::Expr::Base );

use Class::MethodMaker (
    get_set => [qw( -java Key Ref )]
);

sub eval
{
    my $self = shift;

    my $hash = $self->getRef->eval;

    return $hash->{$self->getKey->eval};
}

sub perl
{
    my $self = shift;

    my $hash_perl = $self->getRef->perl;

    my $index = $self->getKey->perl;
    return "($hash_perl)->{$index}";
}

1;
