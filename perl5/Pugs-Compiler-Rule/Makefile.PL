use strict;
#use lib '../..', '../../inc';
#use inc::Module::Install (
#    -d '../../misc/pX' ? (prefix => '../../inc') : ()
#);
use lib '../../../..', '../../../../inc';
use inc::Module::Install (
    -d '../../../../misc/pX' ? (prefix => '../../../../inc') : ()
);
use Cwd qw< abs_path cwd >;

my $cwd = cwd();
my $pm  = abs_path('lib/Pugs/Grammar/Rule/Rule.pm');
my $pmc = $pm . 'c';

my $lrep_dir = "./lrep";

my $pmc_is_uptodate = (-s $pmc and (-M $pmc <= -M $pm));

if (-d $lrep_dir and not $pmc_is_uptodate) {
    chdir $lrep_dir;
    system("$^X p6compiler.pl --print-program $pm > $pmc");
    chdir $cwd;
}

my $mtime = time;
utime $mtime, $mtime, $pmc;

    name('Pugs-Compiler-Rule');
all_from('lib/Pugs/Compiler/Rule.pm');
requires('Parse::Yapp' => '0');
requires('PadWalker' => '1.0');

&WriteAll;

sub MY::postamble {
    # my $blib_pmc = $pmc;
    # $blib_pmc =~ s!(.*)lib!$1blib/lib!;
    << ".";
config ::
\t-\$(NOECHO) \$(CHMOD) 644 $pmc
\t-\$(NOECHO) \$(TOUCH) $pmc
.
}

