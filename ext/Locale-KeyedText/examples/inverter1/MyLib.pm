use v6-alpha;

use Locale::KeyedText;

###########################################################################
###########################################################################

module MyLib {
    sub my_invert (Str $number!) returns Num {
        die Locale::KeyedText::Message.new(
                msg_key => 'MYLIB_MYINV_NO_ARG' )
            if !$number.defined;
        die Locale::KeyedText::Message.new(
                msg_key => 'MYLIB_MYINV_BAD_ARG',
                msg_vars => { 'GIVEN_VALUE' => $number },
            )
            if $number !~~ m/^-?(\d+\.?|\d*\.\d+)$/; # integer or decimal
        die Locale::KeyedText::Message.new(
                msg_key => 'MYLIB_MYINV_RES_INF' )
            if $number == 0;
        return 1 / $number;
    }
} # module MyLib

###########################################################################
###########################################################################
