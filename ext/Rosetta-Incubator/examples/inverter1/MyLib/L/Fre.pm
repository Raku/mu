#!/usr/bin/pugs
use v6;

my Str %text_strings is readonly = (
    'MYLIB_MYINV_NO_ARG' => q[my_invert(): paramètre $number est manquant],
    'MYLIB_MYINV_BAD_ARG'
        => q[my_invert(): paramètre $number est ne nombre,]
           ~ q[ il est "<GIVEN_VALUE>"],
    'MYLIB_MYINV_RES_INF'
        => q[my_invert(): aboutir a est infini parce que]
           ~ q[ paramètre $number est zero],
);

module MyLib::L::Fre {
    sub get_text_by_key (Str $msg_key) returns Str {
        return %text_strings{$msg_key};
    }
} # module MyLib::L::Fre
