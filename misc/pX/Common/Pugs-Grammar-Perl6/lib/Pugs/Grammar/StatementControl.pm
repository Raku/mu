package Pugs::Grammar::StatementControl;
use strict;
use warnings;
use base qw(Pugs::Grammar::BaseCategory);

# TODO - implement the "magic hash" dispatcher
# TODO - generate AST

BEGIN {
    __PACKAGE__->add_rule( if =>    q( ... { return { xxx => $() ,} } ) );
    __PACKAGE__->add_rule( while => q( ... { return { xxx => $() ,} } ) );
    __PACKAGE__->recompile;
}


1;
