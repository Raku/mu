package Pugs::Grammar::StatementControl;
use strict;
use warnings;
use base qw(Pugs::Grammar::BaseCategory);

# TODO - implement the "magic hash" dispatcher
# TODO - generate AST
# TODO - redefine <ws> to test Pod.pm after each \n

sub register_sub {
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
}

BEGIN {
    __PACKAGE__->add_rule(
        '{' => q( { return { stmt => '{'} } ));
    __PACKAGE__->add_rule(
        '}' => q( { return { stmt => '}'} } ));
    __PACKAGE__->add_rule(
        '->' => q( { return { stmt => '->'} } ));
    __PACKAGE__->add_rule(
        '.' => q( <before <Pugs::Grammar::Term.ident> > { return { stmt => '.'} } ));
    for ( qw( 
        my our has
    ) ) {
        __PACKAGE__->add_rule(
            $_ =>  qq( <before \\s> { return { stmt => '$_' } } ));
    }
    __PACKAGE__->recompile;
}


1;
