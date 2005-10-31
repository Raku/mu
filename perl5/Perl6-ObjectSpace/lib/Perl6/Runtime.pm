
package Perl6::Runtime;

use strict;
use warnings;

use Perl6::Core::Closure;

our $TOP_LEVEL_ENV = closure::env->new();

sub get_top_level_env { $TOP_LEVEL_ENV } 

1;

__END__