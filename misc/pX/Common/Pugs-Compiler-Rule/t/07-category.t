use Test::More tests => 2;
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

    #print "cat: ", Dumper($cat);
    #print "grammar in perl6: \n", $cat->emit_grammar_perl6();
    #print "grammar in perl5: \n", $cat->emit_grammar_perl5();

    {
        package test;
        use Pugs::Compiler::Rule;
        use Pugs::Grammar::Base;
        use Data::Dumper;
        no warnings qw( once );
        *item = Pugs::Compiler::Rule->compile( '\d+' )->code;
        eval $cat->emit_grammar_perl5();
        my $match = test->parse( '3+5*6' );
        #print Dumper( $match );
        #show_match( $match );
        Test::More::is( "$match", "3+5*6", "expression matches" );
    }
}

sub test::show_match {
    my @v;
    if ( ref( $_[0] ) eq 'ARRAY' ) {
        for ( @{$_[0]} ) {
            test::show_match( $_ );
        }
        return;
    }
    print ref($_[0]);
    @v = %{$_[0]};
    while ( @v ) {
        my $k = shift @v;
        my $v = shift @v;
        print "\n$k => $v ";
        if ( ref($v) ) { test::show_match( $v ) } else { print $v };
    }
}
