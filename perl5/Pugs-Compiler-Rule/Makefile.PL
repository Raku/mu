#!/usr/bin/perl

use strict;
use warnings;

use inc::Module::Install;
use Cwd qw< abs_path cwd >;

my @inc;
BEGIN {
    @inc = (-e '../../inc' and -e '../../VICTUALS')
        ? (prefix => '../../inc')
        : (-e '../../../../inc' and -e '../../../../VICTUALS')
        ? (prefix => '../../../../inc')
        : ()
}
use lib '../..', '../../../..', (@inc ? $inc[-1] : ());

my $pmc  = abs_path('lib/Pugs/Grammar/Rule.pmc');

my $mtime = time;
utime $mtime, $mtime, $pmc;

name('Pugs-Compiler-Rule');
all_from('lib/Pugs/Compiler/Rule.pm');
requires('Parse::Yapp' => '0');
requires('PadWalker' => '1.0');
requires('Cache::Cache' => '1.05');
recommends('YAML::Syck' => '0.60');

use_test_base();
tests('t/*.t t/*/*.t');

WriteAll();

sub MY::postamble {
    # my $blib_pmc = $pmc;
    # $blib_pmc =~ s!(.*)lib!$1blib/lib!;
    << ".";
config ::
\t-\$(NOECHO) \$(CHMOD) 644 $pmc
\t-\$(NOECHO) \$(TOUCH) $pmc
.
}

# clean the precompiled rules cache
eval {
    require Cache::FileCache;
    my $cache = new Cache::FileCache( { 'namespace' => 'v6-rules' } );
    $cache->Clear;
};
