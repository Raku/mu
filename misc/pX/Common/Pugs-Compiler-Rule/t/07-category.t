use Test::More tests => 4;
use strict;
use warnings;
use Data::Dumper;
use_ok( "Pugs::Grammar::Category" );

{
    my $cat = Pugs::Grammar::Category->new( {
        name => 'test',
        operand => 'item',
    } );
    $cat->add_op( {
        name => '+',
        block => sub {},
        assoc => 'left',
        precedence => 'looser',
        other => '*',
        fixity => 'infix',
    } );
    $cat->add_op( {
        name => '*',
        block => sub {},
        assoc => 'left',
        precedence => 'tighter',
        other => '+',
        fixity => 'infix',
    } );
    $cat->add_op( {
        name => '-',
        block => sub {},
        assoc => 'left',
        precedence => 'equal',
        other => '+',
        fixity => 'infix',
    } );
    $cat->add_op( {
        name => 'or',
        block => sub {},
        assoc => 'left',
        precedence => 'looser',
        other => '+',
        fixity => 'infix',
    } );
    $cat->add_op( {
        name => '[',
        name2 => ']',
        block => sub {},
        assoc => 'left',
        precedence => 'looser',
        other => '+',
        fixity => 'postcircumfix',
    } );
    $cat->add_op( {
        name => 'Y',
        block => sub {},
        assoc => 'list',
        precedence => 'looser',
        other => '+',
        fixity => 'infix',
    } );
    $cat->add_op( {
        name => 'custom_op',
        block => sub {},
        assoc => 'left',
        precedence => 'looser',
        other => '+',
        fixity => 'infix',
        rule => '<custom_parsed>',
    } );
    $cat->add_op( {
        name => '??',
        name2 => '!!',
        block => sub {},
        assoc => 'left',
        precedence => 'equal',
        other => 'custom_op',
        fixity => 'ternary',
    } );
    $cat->add_op( {
        name => '(',
        name2 => ')',
        block => sub {},
        assoc => 'left',
        precedence => 'equal',
        other => '*',
        fixity => 'circumfix',
    } );

    #print "cat: ", Dumper($cat);
    #print "grammar in perl6: \n", $cat->emit_grammar_perl6();
    #print "grammar in perl5: \n", $cat->emit_grammar_perl5();

    package test;
    use Pugs::Compiler::Rule;
    use Pugs::Grammar::Base;
    use Data::Dumper;
    no warnings qw( once );
    *item = Pugs::Compiler::Rule->compile( '\d+' )->code;
    eval $cat->emit_grammar_perl5();

    {
        my $match = test->parse( '3+5*6' );
        #print show_match( $match );
        Test::More::is( "$match", "3+5*6", "expression matches" );
    }

    {
        my $match = test->parse( '(2)' );
        #print show_match( $match );
        Test::More::is( "$match", "(2)", "expression matches" );
    }

    {
        my $match = test->parse( '4*(3+5*6)' );
        #print show_match( $match );
        Test::More::is( "$match", "4*(3+5*6)", "expression matches" );
    }
}

sub test::show_match {
    my $m = shift;
    my $tab = shift || "";
    if ( @$m ) {
        return join('', map {test::show_match( $_, $tab."  " )} @$m );
    }
    if ( %$m ) {
      my $ret;
      my %h = %$m;
      while (my ($k, $v) = each %h) {
        $ret .= "$tab $k => \n" . test::show_match( $v, $tab."  " );
      }
      return $ret;
    }
    return "$tab $m\n";
}
