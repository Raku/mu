=for notes
* synopsis
 BEGIN {require "Filtered-Comments.pm";import Filtered::Comments 'regex matching subs you want to debug'}
* adds use Smart::Comments (if it matches the supplied regex) or
no Smart::Comments before every fully qualified subroutine.
* takes a rexep as the only argument
* not tested much
* read this modules source before using
* examine the proccesed source code by hand if getting strange results by passin
	all => sub {print}
to FILTER_ONLY

=cut

package Filtered::Comments;
use Filter::Simple;
sub import {
	$regex = $_[1];	
}
FILTER_ONLY
code => sub {
	s{^(sub \s+ (\w+::\w+) \s* \{)}{
		$orginal = $1;
		$sub = $2;
		if ($sub =~ /$regex/) {
			"warn 'Filtered:$sub';\nuse Smart::Comments;\n" . $orginal
		} else {
			"no Smart::Comments;\n" . $orginal;
		}
	}egmx;
}

