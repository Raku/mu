my $foo = sub {
}
sub join($sep,$array) {
    my $ret;
    map(sub ($x) {
        if $ret {
            $ret = $ret ~ $sep ~ $x;
        } else {
            $ret = $x;
        }
    },$array);
    $ret;
}

role Mold {
    method ACCEPTS($thing) {
        PRIMITIVES::ritest((|$thing),PRIMITIVES::SMOP_RI($foo.mold));
    }
}

role idconst {
    method ACCEPTS($thing) {
        PRIMITIVES::ritest((|$thing),PRIMITIVES::SMOP_RI('foo'));
    }
}

multi to_c(Mold $mold) {
    "SMOP__Mold__create(" ~
        ($mold.registers-$mold.constants.elems) ~ ","
        ~ "(SMOP__Object*[]) \{" ~ join(',',map(sub ($c) {to_c($c)},$mold.constants)) ~ ",NULL\},"
        ~ $mold.opcodes.elems ~ ",(int[]) \{" ~ join(',',$mold.opcodes)
    ~ "\})"
}
multi to_c(int $int) {
    "SMOP__NATIVE__int_create($int)";
}

multi to_c(idconst $str) {
    "SMOP__NATIVE__idconst_createn(\"" ~ $str ~ "\"," ~ $str.bytes ~ ")";
}

multi to_c($other) {
    "...";
}

say to_c($foo.mold);
