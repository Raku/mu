use Parse::Yapp;
$DEBUG=1;
$|=1;


my $g = <<'EOT';
%{ my $out; %}
%left '*'
%%
S:  A { return($out) } ;
A:  A '*' A { $out="($_[1]$_[2]$_[3])" }
  | B
;
B:  'a' | 'b' | 'c' | 'd' ;
%%
EOT


my $in = [ 'a', '*', 'b', '*', 'c', '*', 'd' ];


my $chk = "(((a*b)*c)*d)";


my($count)=0;


    my($lex) = sub {
        my($t)=shift(@$in);

            defined($t)
        or  $t='';
        return($t,$t);
    };


    my($p)=new Parse::Yapp(input => $g);
    $p=$p->Output(classname => 'Test');

        $DEBUG
    and print $p;

    eval $p;
        $@
    and do {
        print "$@\n";
        die;
    };

    $p=new Test(yylex => $lex, yyerror => sub {});

    $out=$p->YYParse;
    undef $p;

        $out eq $chk
    or  do {
        print "Got '$out' instead of '$chk'\n";
        die;
    };

    undef(&Test::new);

