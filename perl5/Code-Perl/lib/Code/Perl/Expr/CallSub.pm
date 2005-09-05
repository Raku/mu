# $Header: /home/fergal/my/cvs/Code-Perl/lib/Code/Perl/Expr/CallSub.pm,v 1.4 2003/06/17 18:11:41 fergal Exp $

use strict;

package Code::Perl::Expr::CallSub;

use base 'Code::Perl::Expr::Base';

use Class::MethodMaker (
    get_set => [qw( -java SubName Args )]
);

sub eval
{
    my $self = shift;

    my $subname = $self->getSubName->eval;
    my $args = $self->getArgs;

    no strict 'refs';
    return &{$subname}($args->eval);
}

sub perl
{
    my $self = shift;

    my $subname = $self->getSubName;
    my $subname_perl = $subname->perl;
    my $args = $self->getArgs->perl;

    return ref($subname) eq "Code::Perl::Expr::SubName" ?
        "$subname_perl($args)" :
        "&{$subname_perl}($args)";
}

1;
