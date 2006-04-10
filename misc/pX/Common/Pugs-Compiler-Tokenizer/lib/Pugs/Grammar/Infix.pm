package Pugs::Grammar::Infix;
use strict;
use warnings;
use base qw(Pugs::Grammar::BaseCategory);

BEGIN {
    __PACKAGE__->add_rule( 'infix:<+>' => '{ return { op => "infix:<+>" ,} }' );
    __PACKAGE__->add_rule( '+'         => '{ return { op => "infix:<+>" ,} }' );
    __PACKAGE__->add_rule( 'infix:<->' => '{ return { op => "infix:<->" ,} }' );
    __PACKAGE__->add_rule( '-'         => '{ return { op => "infix:<->" ,} }' );
    __PACKAGE__->add_rule( 'infix:<*>' => '{ return { op => "infix:<*>" ,} }' );
    __PACKAGE__->add_rule( '*'         => '{ return { op => "infix:<*>" ,} }' );
    __PACKAGE__->add_rule( 'infix:</>' => '{ return { op => "infix:</>" ,} }' );
    __PACKAGE__->add_rule( '/'         => '{ return { op => "infix:</>" ,} }' );
    __PACKAGE__->recompile;
}

1;
