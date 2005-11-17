
package Perl6::Runtime;

use strict;
use warnings;

use Perl6::Core::Closure;

$::ENV = closure::env->new();

sub get_top_level_env { $::ENV } 

1;

__END__