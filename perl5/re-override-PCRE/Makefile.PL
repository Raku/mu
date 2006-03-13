use strict;
use lib '.', '../..';
use inc::Module::Install prefix => '../../inc';
use File::Copy;
use File::Basename;

if (not -e 'pcre.h' and -e '../../src/pcre/pcre.h') {
    for my $file (glob("../../src/pcre/*")) {
        copy($file => basename($file));
    }
}

if (not -e "pcre_wrap.c") {
    system(
        'swig',
        qw(-noproxy -module re::override::PCRE -includeall -exportall -perl5 pcre.i)
    )
}

name            ('re-override-PCRE');
all_from        ('lib/re/override/PCRE.pm');
cc_inc_paths    ('.');
cc_files        (grep { !/printint|ucp(?:type)?table/ } glob("*.c"));

#cc_optimize_flags '-g3';

can_cc() or die "This module requires a C compiler";

&WriteAll( sign => 1 );
