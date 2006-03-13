use strict;
use lib '.', '../..';
use inc::Module::Install prefix => '../../inc';
use File::Copy;
use File::Basename;

if (not -e 'ppport.h') {
    require Devel::PPPort;
    Devel::PPPort::WriteFile();
}

name            ('re-override');
all_from        ('lib/re/override.pm');

#cc_optimize_flags '-g3';

can_cc() or die "This module requires a C compiler";

&WriteAll( sign => 1 );
