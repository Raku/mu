use Test::More tests => 2;
use strict;
use warnings;
use Data::Dumper;
use_ok( "Pugs::Grammar::Precedence" );
use Parse::Yapp;

{
    my $cat = Pugs::Grammar::Precedence->new();
    $cat->add_op( {
        name => '+',
        block => sub {},
        assoc => 'left',
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


    my $g = $cat->emit_yapp();

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

    my($p)=new Parse::Yapp(input => $g);
    $p=$p->Output(classname => 'Test');
    #print $p;

    eval $p;
    die "$@\n" if $@;

    $p=new Test(yylex => $lex, yyerror => sub { die "error" });

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
