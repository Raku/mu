use strict;
use lib '.';
use inc::Module::Install;

name            're-override';
all_from        'lib/re/override.pm';
cc_inc_paths    '.';
cc_files        (grep { !/printint|ucp(?:type)?table/ } glob("*.c"));

cc_optimize_flags '-g3';

can_cc or die "This module requires a C compiler";

sign; WriteAll;
