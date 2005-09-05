# $Header: /home/fergal/my/cvs/Code-Perl/lib/Code/Perl/Expr/String.pm,v 1.1 2003/06/17 14:14:21 fergal Exp $

use strict;

package Code::Perl::Expr::String;

use base 'Code::Perl::Expr::Constant';

# inherits eval

sub perl
{
    my $self = shift;

    return $self->getQuotedValue;
}

1;
