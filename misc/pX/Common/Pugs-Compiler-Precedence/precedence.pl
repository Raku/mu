use Parse::Yapp;
$DEBUG=0;
$|=1;
use Data::Dumper;


my $g = <<'EOT';
%{ my $out; %}
%left   '-' '+'
%left   '*' '/'
%%
S:  A { return($out) } ;
A:  A '*' A { $out= [ $_[1], $_[2], $_[3] ] }
  | A '/' A { $out= [ $_[1], $_[2], $_[3] ] }
  | A '+' A { $out= [ $_[1], $_[2], $_[3] ] }
  | A '-' A { $out= [ $_[1], $_[2], $_[3] ] }
  | B
;
B:  'a' | 'b' | 'c' | 'd' ;
%%
EOT


my $in = [ 
    ['a'=>{term=>'a'}], 
    ['*'=>{op=>'*'}], 
    ['b'=>{term=>'b'}], 
    ['+'=>{op=>'+'}], 
    ['c'=>{term=>'c'}], 
    ['*'=>{op=>'*'}], 
    ['d'=>{term=>'d'}] 
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

