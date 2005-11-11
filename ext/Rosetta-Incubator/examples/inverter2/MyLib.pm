#!/usr/bin/pugs
use v6;

use Locale::KeyedText;

module MyLib {
    sub my_invert (Str $number) returns Num {
        throw Locale::KeyedText::Message.new(
                'msg_key' => 'MYLIB_MYINV_NO_ARG' )
            if !$number.defined;
        throw Locale::KeyedText::Message.new(
                'msg_key' => 'MYLIB_MYINV_BAD_ARG',
                'msg_vars' => { 'GIVEN_VALUE' => $number },
            )
            if $number !~ m/\d/;
        throw Locale::KeyedText::Message.new(
                'msg_key' => 'MYLIB_MYINV_RES_INF' )
            if $number == 0;
        return 1 / $number;
    }
} # module MyLib

###########################################################################
###########################################################################

my Str %text_stringsE is readonly = (
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
        return %text_stringsE{$msg_key};
    }
} # module MyLib::L::Eng

###########################################################################
###########################################################################

my Str %text_stringsF is readonly = (
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
        return %text_stringsF{$msg_key};
    }
} # module MyLib::L::Fre
