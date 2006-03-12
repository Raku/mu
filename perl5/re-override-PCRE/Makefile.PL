use strict;
use lib '.';
use inc::Module::Install;
use File::Copy;
use File::Basename;

if (not -e 'pcre.h' and -e '../../src/pcre/pcre.h') {
    for my $file (glob("../../src/pcre/*")) {
        copy($file => basename($file));
    }
}

if (not -e 'ppport.h') {
    require Devel::PPPort;
    Devel::PPPort::WriteFile();
}

name            're-override';
all_from        'lib/re/override.pm';
cc_inc_paths    '.';
cc_files        (grep { !/printint|ucp(?:type)?table/ } glob("*.c"));

#cc_optimize_flags '-g3';

can_cc or die "This module requires a C compiler";

sign; WriteAll;
