#!pugs
use v6;

# This module is used when testing Locale::KeyedText.
# It contains some utility methods used by the various LKT_*.t scripts.

module t_LKT_Util;

######################################################################

sub message( Str $detail is rw ) {
    say "# $detail";
}

######################################################################

sub serialize( Any $input is rw ) returns Str {
    return [
        !$input.defined ??
            'undef, '
        !! $input.does(Hash) ?? 
            ( '{ ', ( $input.pairs.sort.map:{ serialize( $_ ) } ), '}, ' ) 
        !! $input.does(Pair) || $input.ref eq 'Array::Const' ?? # Slice not does(Pair) right now
            '\''~$input.key~'\' => \''~$input.value~'\', '
        !! $input.does(Array) ?? 
            ( '[ ', ( $input.map:{ serialize( $_ ) } ), '], ' ) 
        !! '\''~$input~'\', '
    ].join( '' );
}

######################################################################
