say "1..1";
$LexicalPrelude.{'&infix:+:(int,int)'} = sub {
    say "ok 1";
}
&infix:<+>:(int,int)("ok 2");
