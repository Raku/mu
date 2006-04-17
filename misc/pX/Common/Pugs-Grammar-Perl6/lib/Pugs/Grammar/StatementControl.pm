package Pugs::Grammar::StatementControl;
use strict;
use warnings;
use base qw(Pugs::Grammar::BaseCategory);

# TODO - implement the "magic hash" dispatcher
# TODO - generate AST
# TODO - redefine <ws> to test Pod.pm after each \n

sub register_sub {
    #~ $_[0] == 
        #~ 'pair' => {
          #~ 'key' => {
            #~ 'single_quoted' => 'prefix'
          #~ },
          #~ 'value' => {
            #~ 'single_quoted' => 'xx'
          #~ }
        #~ }    
    #if ( $_[0]{pair} ) {
        my $category = $_[0]{pair}{key}{single_quoted};
        my $name     = $_[0]{pair}{value}{single_quoted};
        $category =~ s/^(\w)/uc($1)/e; 
        $category = 'Pugs::Grammar::' . $category;
        $category->add_rule( 
            name => $name,
            assoc => 'left',
            precedence => 'equal',
            other => '+',
        );
    #~ }
    #~ use Data::Dumper;
    #~ print Dumper( $_[0]{pair} );
    #~ die "not a pair";
}

BEGIN {
    __PACKAGE__->add_rule(
        ';' => q( { return { stmt => ';'} } ));
    __PACKAGE__->add_rule(
        '{' => q( { return { stmt => '{'} } ));
    __PACKAGE__->add_rule(
        '}' => q( { return { stmt => '}'} } ));
    __PACKAGE__->add_rule(
        '.' => q( <before <Pugs::Grammar::Term.bareword> > { return { stmt => '.'} } ));
    __PACKAGE__->add_rule(
        'sub' => q( <before \s> { return { stmt => 'sub'} } ));
    __PACKAGE__->add_rule(
        'multi' => q( <before \s> { return { stmt => 'multi'} } ));
    for ( qw( 
        for 
        if else elsif 
        while 
        BEGIN END 
    ) ) {
        __PACKAGE__->add_rule(
            $_ =>  qq( { return { stmt => '$_' } } ));
    }
    __PACKAGE__->recompile;
}


1;
