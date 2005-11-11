#!/usr/bin/pugs
use v6;

my Str %text_strings is readonly = (
    'MYLIB_MYINV_NO_ARG' => q[my_invert(): argument $number is missing],
    'MYLIB_MYINV_BAD_ARG'
        => q[my_invert(): argument $number is not a number,]
           ~ q[ it is "<GIVEN_VALUE>"],
    'MYLIB_MYINV_RES_INF'
        => q[my_invert(): result is infinite because]
           ~ q[ argument $number is zero],
);

module MyLib::L::Eng {
    sub get_text_by_key (Str $msg_key) returns Str {
        return %text_strings{$msg_key};
    }
} # module MyLib::L::Eng
