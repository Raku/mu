#!/usr/bin/perl

use strict;
use warnings;
my @inc;
BEGIN {
    @inc = (-e '../../inc' and -e '../../VICTUALS')
        ? (prefix => '../../inc')
        : (-e '../../../../inc' and -e '../../../../VICTUALS')
        ? (prefix => '../../../../inc')
        : ()
}
use lib '../..', (@inc ? $inc[-1] : ());
use inc::Module::Install @inc;

use Cwd qw< abs_path cwd >;

my $cwd = cwd();
my $pm  = abs_path('lib/Pugs/Grammar/Rule/Rule.pm');
my $pmc = $pm . 'c';

my $mtime = time;
utime $mtime, $mtime, $pmc;

    name('Pugs-Compiler-Rule');
all_from('lib/Pugs/Compiler/Rule.pm');
requires('Parse::Yapp' => '0');
requires('PadWalker' => '1.0');
requires('Class::InsideOut' => '0');
requires('Data::Dump::Streamer' => '0');
requires('Cache::Cache' => '1.05');

&WriteAll;

sub MY::postamble {
    # my $blib_pmc = $pmc;
    # $blib_pmc =~ s!(.*)lib!$1blib/lib!;
    << ".";
config :: $pmc

$pmc: $pm
\t(cd lrep && $^X p6compiler.pl --output-program=$pmc $pm && cd..)
\t-\$(NOECHO) \$(CHMOD) 644 $pmc
.
}

# clean the precompiled rules cache
eval {
    require Cache::FileCache;
    my $cache = new Cache::FileCache( { 'namespace' => 'v6-rules' } );
    $cache->Clear;
};
