package PIL::RawJS;

use warnings;
use strict;

sub new { bless \do { my $a = $_[1] } => $_[0] }

sub as_js { ${ $_[0] } }

1;
