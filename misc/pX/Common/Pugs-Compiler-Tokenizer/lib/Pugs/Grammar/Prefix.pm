package Pugs::Grammar::Prefix;
use strict;
use warnings;
use base qw(Pugs::Grammar::BaseCategory);

# TODO - generate AST
# TODO - prefix:{'+'}
# TODO - ~ ? 

BEGIN {
    __PACKAGE__->add_rule( 'prefix:<+>' => '{ return { op => "prefix:<+>" ,} }' );
    __PACKAGE__->add_rule( '+'          => '{ return { op => "prefix:<+>" ,} }' );
    __PACKAGE__->add_rule( 'prefix:<->' => '{ return { op => "prefix:<->" ,} }' );
    __PACKAGE__->add_rule( '-'          => '{ return { op => "prefix:<->" ,} }' );
    __PACKAGE__->recompile;
}

1;
