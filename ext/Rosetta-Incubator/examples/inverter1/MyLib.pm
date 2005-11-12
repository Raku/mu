#!/usr/bin/pugs
use v6;

use Locale::KeyedText;

###########################################################################
###########################################################################

module MyLib {
    sub my_invert (Str $number) returns Num {
        die Locale::KeyedText::Message.new(
                'msg_key' => 'MYLIB_MYINV_NO_ARG' )
            if !$number.defined;
        die Locale::KeyedText::Message.new(
                'msg_key' => 'MYLIB_MYINV_BAD_ARG',
                'msg_vars' => { 'GIVEN_VALUE' => $number },
            )
#            if $number !~ m/^-?(\d+\.?|\d*\.\d+)$/; # integer or decimal
            if $number !~ m:perl5/^-?(\d+\.?|\d*\.\d+)$/; #: inte or deci
        die Locale::KeyedText::Message.new(
                'msg_key' => 'MYLIB_MYINV_RES_INF' )
            if $number == 0;
        return 1 / $number;
    }
} # module MyLib

###########################################################################
###########################################################################
