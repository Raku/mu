use Test::More tests => 2;
use strict;
use warnings;
use Data::Dumper;
$Data::Dumper::Indent = 1;
$Data::Dumper::Sortkeys = 1;
use_ok( "Pugs::Grammar::Precedence" );
use Parse::Yapp;

{
    my $cat = Pugs::Grammar::Precedence->new( grammar => 'test' );
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


    my $p = $cat->emit_grammar_perl5;
    #print $p;
    eval $p;
    die "$@\n" if $@;

    my $in = [ 
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
    $p=new test(yylex => $lex, yyerror => sub { die "error" });

    my $out=$p->YYParse;
    #print Dumper $out;
    
    my $expected = {
      'exp2' => {
        'exp2' => {
          'num' => '4'
        },
        'exp1' => {
          'num' => '3'
        },
        'op1' => '*'
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

    is_deeply( $out, $expected, '1*2+3*4' );
}
