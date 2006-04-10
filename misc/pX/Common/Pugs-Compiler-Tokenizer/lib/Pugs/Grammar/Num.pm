package Pugs::Grammar::Num;
use strict;
use warnings;
use base qw(Pugs::Grammar::BaseCategory);

=for pod

Parses text like:

    1
    1.1
    +1
    1e10

=cut

# TODO - implement the "magic hash" dispatcher
# TODO - generate AST

BEGIN {
    __PACKAGE__->add_rule( q() => q( \d+ { return { num => $() ,} } ) );
    __PACKAGE__->add_rule( Inf => q({ return { num => 'Inf' ,} } ) );
    __PACKAGE__->add_rule( NaN => q({ return { num => 'NaN' ,} } ) );
    __PACKAGE__->recompile;
}


1;
