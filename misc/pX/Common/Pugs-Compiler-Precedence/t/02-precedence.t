use Test::More tests => 1; #4;
use strict;
use warnings;
use Data::Dumper;
use_ok( "Pugs::Grammar::Precedence" );
use Parse::Yapp;

{
    my $cat = Pugs::Grammar::Precedence->new( {
        name => 'test',
        operand => 'item',
    } );
    $cat->add_op( {
        name => '+',
        block => sub {},
        assoc => 'left',
        #precedence => 'looser',
        #other => '*',
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
        #assoc => 'non',
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
        #assoc => 'left',
        precedence => 'looser',
        other => '+',
        fixity => 'infix',
        rule => '<custom_parsed>',
    } );
    $cat->add_op( {
        name => '??',
        name2 => '!!',
        block => sub {},
        #assoc => 'left',
        precedence => 'equal',
        other => 'custom_op',
        fixity => 'ternary',
    } );
    $cat->add_op( {
        name => '(',
        name2 => ')',
        block => sub {},
        #assoc => 'left',
        precedence => 'equal',
        other => '*',
        fixity => 'circumfix',
    } );


    my $g = $cat->emit_yapp();

    my $in = [ 
        ['-'  =>{op=>'-'}], 
        ['NUM'=>{num=>'1'}], 
        ['*'  =>{op=>'*'}], 
        ['NUM'=>{num=>'2'}], 
        ['+'  =>{op=>'+'}], 
        ['NUM'=>{num=>'3'}], 
        ['*'  =>{op=>'*'}], 
        ['NUM'=>{num=>'4'}] 
    ];
    my($lex) = sub {
        my($t)=shift(@$in);
            defined($t)
        or  $t=['',''];
        return($$t[0],$$t[1]);
    };

    my($p)=new Parse::Yapp(input => $g);
    $p=$p->Output(classname => 'Test');
    #print $p;

    eval $p;
    die "$@\n" if $@;

    $p=new Test(yylex => $lex, yyerror => sub { die "error" });

    my $out=$p->YYParse;

    print Dumper $out;

    undef $p;
    undef(&Test::new);
}

__END__

    #print "cat: ", Dumper($cat);
    #print "grammar in perl6: \n", $cat->emit_grammar_perl6();
    #print "grammar in perl5: \n", $cat->emit_grammar_perl5();

    package test;
    use Pugs::Compiler::Rule;
    use Pugs::Grammar::Base;
    use Data::Dumper;
    no warnings qw( once );

    eval $cat->emit_grammar_perl5();

# tests temporarily disabled
}
__END__

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
