say "1..1";
my &infix:<+>:(int,int);
&infix:<+>:(int,int) := sub {
    say "ok 1";
}
&infix:<+>:(int,int)("ok 2");
