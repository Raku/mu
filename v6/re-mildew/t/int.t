say "1..8";
if &infix:<==>:(int,int)(1788,1788) {
    say "ok 1 # infix:<==>";
} else {
    say "not ok 1 # infix:<==>";
}

if &infix:<==>:(int,int)(1788,1787) {
    say "not ok 2 # infix:<==>";
} else {
    say "ok 2 # infix:<==>";
}

if &infix:<==>:(int,int)(&infix:<+>:(int,int)(1788,2),1790) {
    say "ok 3 # infix:<+>";
} else {
    say "not ok 3 # infix:<+>",;
}

if &infix:<==>:(int,int)(&infix:<->:(int,int)(600,100),500) {
    say "ok 4 # infix:<->";
} else {
    say "not ok 4 # infix:<->",;
}

if &infix:<==>:(int,int)(4,5) {
    say "not ok 5";
} else {
    say "ok 5";
}
if &infix:<==>:(int,int)(6,6) {
    say "ok 6";
} else {
    say "not ok 6";
}

my $tmp = 0.Str;
say "ok 7 # lives after stringifing 0";

if &infix:<->:(int,int)(0,-10) eq '-10' {
    say "ok 8 # stringifing -10";
} else {
    say "not ok 8 # stringifing -10";
}
