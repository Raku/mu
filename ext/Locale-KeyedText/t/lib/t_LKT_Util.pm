#!pugs
use v6;

# This module is used when testing Locale::KeyedText.
# It contains some utility methods used by the various LKT_*.t scripts.

module t_LKT_Util;

######################################################################

sub message( Str $detail ) {
	say "# $detail";
}

######################################################################

sub serialize( Any $input ) returns Str {
	return [
		$input.does(Hash) ?? 
			( '{ ', ( $input.pairs.sort.map:{ serialize( $_ ) } ), '}, ' ) 
		:: $input.does(Array) ?? 
			( '[ ', ( $input.map:{ serialize( $_ ) } ), '], ' ) 
		:: $input.does(Pair) ?? 
			'\''~$input.key~'\' => \''~$input.value~'\', '
		:: $input.defined ??
			'\''~$input~'\', '
		:: 'undef, '
	].join( '' );
}

######################################################################
