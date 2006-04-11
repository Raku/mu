use Test::More tests => 2;
use strict;
use warnings;
use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Sortkeys = 1;
use_ok( "Pugs::Grammar::Precedence" );
use Parse::Yapp;

{
    my $cat = Pugs::Grammar::Precedence->new( 
        grammar => 'test',
    );
    $cat->add_op( {
        name => '+',
        assoc => 'left',
        fixity => 'infix',
    } );
    $cat->add_op( {
        name => '*',
        assoc => 'left',
        precedence => 'tighter',
        other => '+',
        fixity => 'infix',
    } );
    $cat->add_op( {
        name => '-',
        assoc => 'left',
        precedence => 'equal',
        other => '+',
        fixity => 'infix',
    } );
    $cat->add_op( {
        name => 'or',
        assoc => 'left',
        precedence => 'looser',
        other => '+',
        fixity => 'infix',
    } );
    $cat->add_op( {
        name => '[',
        name2 => ']',
        precedence => 'looser',
        other => '+',
        fixity => 'postcircumfix',
    } );
    $cat->add_op( {
        name => 'Y',
        assoc => 'list',
        precedence => 'looser',
        other => '+',
        fixity => 'infix',
    } );
    $cat->add_op( {
        name => 'custom_op',
        precedence => 'looser',
        other => '+',
        fixity => 'infix',
        rule => '<custom_parsed>',
    } );
    $cat->add_op( {
        name => '??',
        name2 => '!!',
        precedence => 'equal',
        other => 'custom_op',
        fixity => 'ternary',
    } );
    $cat->add_op( {
        name => '(',
        name2 => ')',
        precedence => 'equal',
        other => '*',
        fixity => 'circumfix',
    } );


    my $p = $cat->emit_grammar_perl5;
    #print $p;
    eval $p;
    die "$@\n" if $@;

    my $in = [ 
        ['NUM'=>{num=>'1'}], 
        ['*'  =>{op=>'*'}], 
        ['NUM'=>{num=>'2'}], 
        ['+'  =>{op=>'-'}], 
        ['('  =>{op=>'('}], 
        ['NUM'=>{num=>'3'}], 
        ['*'  =>{op=>'+'}], 
        ['NUM'=>{num=>'4'}], 
        [')'  =>{op=>')'}], 
    ];
    my $expr = join('', map{$_->[0]} @$in);
    my($lex) = sub {
        my($t)=shift(@$in);
            defined($t)
        or  $t=['',''];
        return($$t[0],$$t[1]);
    };
    $p=new test(yylex => $lex, yyerror => sub { die "error" });

    my $out=$p->YYParse;
    #print Dumper $out;
    
    my $expected = {
      'exp2' => {
        'op2' => ')',
        'exp1' => {
          'exp2' => {
            'num' => '4'
          },
          'exp1' => {
            'num' => '3'
          },
          'op1' => '*'
        },
        'op1' => '('
      },
      'exp1' => {
        'exp2' => {
          'num' => '2'
        },
        'exp1' => {
          'num' => '1'
        },
        'op1' => '*'
      },
      'op1' => '+'
    };

    is_deeply( $out, $expected, "parse $expr" );

    #print Dumper $out;
    # undef(&test::new);
}

