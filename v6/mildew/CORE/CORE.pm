my sub say(|$capture) {
    my $i = 0;
    loop {
        if &infix:<==>:(int,int)($i,$capture.elems) {
            $OUT.print("\n");
            return;
        } else {
           $OUT.print($capture.positional($i.FETCH).Str);
           $i = &infix:<+>:(int,int)($i.FETCH,1);
        }
    }
}
my sub print($arg) {
    $OUT.print($arg.FETCH);
}
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
$LexicalPrelude{'&say'} := &say;
$LexicalPrelude{'&print'} := &print;

$LexicalPrelude{'&infix:+:(int,int)'} := sub ($a,$b) {
    PRIMITIVES::int_add($a.FETCH,$b.FETCH);
}

$LexicalPrelude{'&infix:<:(int,int)'} := sub ($a,$b) {
    PRIMITIVES::int_less($a.FETCH,$b.FETCH);
}

$LexicalPrelude{'&infix:-:(int,int)'} := sub ($a,$b) {
    PRIMITIVES::int_substract($a.FETCH,$b.FETCH);
}

$LexicalPrelude{'&infix:==:(int,int)'} := sub ($a,$b) {
    PRIMITIVES::int_equal($a.FETCH,$b.FETCH);
}

$LexicalPrelude{'&infix:~'} := sub (|$capture) {
    my $i = 0;
    my $str = '';
    loop {
        if &infix:<==>:(int,int)($i.FETCH,$capture.elems) {
            return $str.FETCH;
        } else {
           $str = PRIMITIVES::idconst_concat($str.FETCH,$capture.positional($i.FETCH).FETCH.Str);
           $i = &infix:<+>:(int,int)($i.FETCH,1);
        }
    }
}
$LexicalPrelude{'&infix:eq'} := sub ($a,$b) {
    PRIMITIVES::idconst_eq($a.Str,$b.Str);
}
$LexicalPrelude{'&infix:ne'} := sub ($a,$b) {
    if PRIMITIVES::idconst_eq($a.Str,$b.Str) {
        ::False;
    } else {
        ::True;
    }
}
$LexicalPrelude{'&postfix:++'} := sub ($a) {
    $a = &infix:<+>:(int,int)($a,1);
}
$LexicalPrelude{'&prefix:++'} := sub ($a) {
    my $old = $a;
    $a = &infix:<+>:(int,int)($a,1);
    $old;
}

::MildewSOLoader.new.load('Return.mildew.so',$LexicalPrelude.FETCH);
::MildewSOLoader.new.load('RoleHOW.mildew.so',$LexicalPrelude.FETCH);
::MildewSOLoader.new.load('Types.mildew.so',$LexicalPrelude.FETCH);
::MildewSOLoader.new.load('Exception.mildew.so',$LexicalPrelude.FETCH);
::MildewSOLoader.new.load('Failure.mildew.so',$LexicalPrelude.FETCH);
::MildewSOLoader.new.load('Signature.mildew.so',$LexicalPrelude.FETCH);


#$LexicalPrelude{'ModuleLoader'} =
::MildewSOLoader.new.load('ModuleLoader.mildew.so',$LexicalPrelude.FETCH).lookup('ModuleLoader');

my $multi_scope = ::MildewSOLoader.new.load('Multi.mildew.so',$LexicalPrelude.FETCH);
#$LexicalPrelude{'Multi'} = $multi_scope.lookup('Multi');

sub not($thing) {
    if $thing {
        ::False;
    } else {
        ::True;
    }
}
$LexicalPrelude{'&prefix:not'} := &not;
$LexicalPrelude{'&prefix:!'} := &not;
$LexicalPrelude{'&not'} := &not;
$LexicalPrelude{'&True'} := sub {::True};
$LexicalPrelude{'&False'} := sub {::False};

$LexicalPrelude{'&prefix:?'} := sub ($bool) {
    if $bool {
        ::True;
    } else {
        ::False
    }
}

::MildewSOLoader.new.load('EXTERNAL.mildew.so',$LexicalPrelude.FETCH);

::MildewSOLoader.new.load('Eval.mildew.so',$LexicalPrelude.FETCH);

::MildewSOLoader.new.load('int.mildew.so',$LexicalPrelude.FETCH);

role Undef {
    method true {
        ::False.FETCH;
    }
}
$LexicalPrelude{'undef'} := Undef.new;
