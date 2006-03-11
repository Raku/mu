use strict;
use lib '../..';
use inc::Module::Install;
use Cwd qw< abs_path cwd >;

my $cwd = cwd();
my $pm  = abs_path('lib/Pugs/Grammar/Rule.pm');
my $pmc = $pm . 'c';

my $lrep_dir = "../../misc/pX/Common/lrep";

my $pmc_is_uptodate = (-s $pmc and (-M $pmc <= -M $pm));

if (-d $lrep_dir and not $pmc_is_uptodate) {
    chdir $lrep_dir;
    system("$^X p6compiler.pl --print-program $pm > $pmc");
    chdir $cwd;
}

name        ('Pugs-Compiler-Rule');
all_from    ('lib/Pugs/Compiler/Rule.pm');

&WriteAll;
