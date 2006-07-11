# this is the prototype for the Pugs-Compiler-Precedence module - fglock

use Parse::Yapp;
$DEBUG=0;
$|=1;
use Data::Dumper;

# see also:
#  http://web.mit.edu/gnu/doc/html/bison_[6,8].html

# notes:
#  'assoc' is one of:  nonassoc / right / left

my $g = <<'EOT';
%{ my $out; %}

%left   ';'
%left   '-' '«+»'
%left   'infix:<times>' '/'
%left   NEG
%right  '^'

%%
statement:  exp { return($out) } ;

exp:        NUM                 { $out= $_[1] }
        |   STMT                { $out= $_[1] }
        |   STMT exp            { $out= [ $_[1], $_[2] ] }
        |   exp ';' STMT            { $out= [ $_[1], $_[3] ] }
        |   exp '«+»' exp         { $out= [ $_[1], $_[2], $_[3] ] }
        |   exp '-' exp         { $out= [ $_[1], $_[2], $_[3] ] }
        |   exp 'infix:<times>' exp         { $out= [ $_[1], $_[2], $_[3] ] }
        |   exp '/' exp         { $out= [ $_[1], $_[2], $_[3] ] }
        |   '-' exp %prec NEG   { $out= [ $_[1], $_[2] ] }
        |   exp '^' exp         { $out= [ $_[1], $_[2], $_[3] ] }
        |   '(' exp ')'         { $out= $_[2] }
;

list_left:  exp                 { $out= [ $_[1] ] }
        |   list_left ',' exp   { $out= [ @{$_[1]}, $_[3] ] }
;

list_right: exp                 { $out= [ $_[1] ] }
        |   exp ';' list_right  { $out= [ $_[1], @{$_[3]} ] }
;

%%
EOT


my $in = [ 
    ['-'=>{op=>'-'}], 
    ['NUM'=>{num=>'1'}], 
    ['infix:<times>'=>{op=>'*'}], 
    ['NUM'=>{num=>'2'}], 
    ['«+»'=>{op=>'+'}], 
    ['NUM'=>{num=>'3'}], 
    ['infix:<times>'=>{op=>'*'}], 
    ['NUM'=>{num=>'4'}] ,
    [';'=>{}] ,
    ['STMT'=>{if=>'block'}] 
];


    my($lex) = sub {
        my($t)=shift(@$in);
            defined($t)
        or  $t=['',''];
        return($$t[0],$$t[1]);
    };


    my($p)=new Parse::Yapp(input => $g);
    $p=$p->Output(classname => 'Test');

        $DEBUG
    and print $p;

    eval $p;
        $@
    and die "$@\n";

    $p=new Test(yylex => $lex, yyerror => sub {});

    $out=$p->YYParse;

    print Dumper $out;

    undef $p;
    undef(&Test::new);

