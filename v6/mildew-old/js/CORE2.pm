$LexicalPrelude{'&postfix:++'} := sub ($a is ref) {
    $a = &infix:<+>:(int,int)($a.FETCH,1);
}
$LexicalPrelude{'&prefix:++'} := sub ($a) {
    my $old = $a;
    $a = &infix:<+>:(int,int)($a,1);
    $old;
}

$LexicalPrelude{'&infix:~'} := sub (|$capture) {
    my $i = 0;
    my $str = '';
    loop {
        if PRIMITIVES::eq($i.FETCH,$capture.elems) {
            return $str.FETCH;
        } else {
           $str = PRIMITIVES::concat($str.FETCH,$capture.positional($i.FETCH).FETCH.Str);
           $i = &infix:<+>:(int,int)($i.FETCH,1);
        }
    }
}
$LexicalPrelude{'&infix:ne'} := sub ($a,$b) {
    if PRIMITIVES::eq($a.FETCH,$b.FETCH) {
        ::False;
    } else {
        ::True;
    }
}
$LexicalPrelude{'&infix:eq'} := sub ($a,$b) {
    PRIMITIVES::eq($a.Str,$b.Str);
}
$LexicalPrelude{'&infix:==:(int,int)'} := sub ($a,$b) {
    PRIMITIVES::eq($a.FETCH,$b.FETCH);
}
$LexicalPrelude{'&infix:+:(int,int)'} := sub ($a,$b) {
    PRIMITIVES::int_add($a.FETCH,$b.FETCH);
}

$LexicalPrelude{'&infix:<:(int,int)'} := sub ($a,$b) {
    PRIMITIVES::int_less($a.FETCH,$b.FETCH);
}

$LexicalPrelude{'&infix:-:(int,int)'} := sub ($a,$b) {
    PRIMITIVES::int_substract($a.FETCH,$b.FETCH);
}

my sub return(|$capture) {
    my $e = ::ControlExceptionReturn.new();
    $e.capture = $capture;
    $e.routine = CALLER::<&?ROUTINE>;
    $e.throw;
}
$LexicalPrelude{'&return'} := &return;

my sub map($expression,$values) {
    my $i = 0;
    my $ret = ::Array.new;
    loop {
        if &infix:<==>:(int,int)($i,$values.elems) {
            return $ret;
        } else {
           $ret.push((|$expression($values[$i.FETCH])));
           $i = &infix:<+>:(int,int)($i.FETCH,1);
        }
    }
}
my sub grep($expression,$values) {
    my $i = 0;
    my $ret = ::Array.new;
    loop {
        if &infix:<==>:(int,int)($i,$values.elems) {
            return $ret;
        } else {
           if ($expression($values[$i.FETCH])) {
              $ret.push($values[$i.FETCH].FETCH);
           } else {
           }
           $i = &infix:<+>:(int,int)($i.FETCH,1);
        }
    }
}
$LexicalPrelude{'&map'} := &map;
$LexicalPrelude{'&grep'} := &grep;
